;;; alarm-clock.el -- a small Emacs alarm clock -*- lexical-binding: t -*-

;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: home automation

;; This file is not part of GNU Emacs.

;; alarm-clock.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; alarm-clock.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide a convenient keypad based interface for displaying time,
;; and waking myself up in the morning.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'eval-server)
(require 'find-func)

(defvar alarm-clock-temperature nil)
(defvar alarm-clock-alarm-time "")

(defvar alarm-clock-alarm-colors '("#888" "#8f8"))
(defvar alarm-clock-alarm-count 0)

(defun alarm-clock-display ()
  (with-current-buffer (get-buffer-create "*alarm-clock*")
    (erase-buffer)
    (alarm-clock-make-svg (format-time-string "%H:%M")
			 alarm-clock-alarm-time
			 (if alarm-clock-temperature
			     (format "%.1f°" alarm-clock-temperature)
			   "no temp")
			 alarm-clock-sleeve
			 720 720)
    (put-text-property (point-min) (point-max) 'keymap nil)))

(defvar alarm-clock-timer nil)
(defvar alarm-clock-temperature-timer nil)
(defvar alarm-clock-sleeve-timer nil)

(defun start-alarm-clock ()
  (when alarm-clock-timer
    (cancel-timer alarm-clock-timer))
  (setq alarm-clock-timer (run-at-time 1 1 #'alarm-clock-display))
  (when alarm-clock-temperature-timer
    (cancel-timer alarm-clock-temperature-timer))
  (setq alarm-clock-temperature-timer
	(run-at-time 2 30 #'alarm-clock-get-temperature))
  (when alarm-clock-sleeve-timer
    (cancel-timer alarm-clock-sleeve-timer))
  (setq alarm-clock-sleeve-timer
	(run-at-time 3 30 #'alarm-clock-get-sleeve))
  (alarm-clock-start-sensor)
  ;;(run-at-time 10 10 #'alarm-clock-check-network)
  )

(defvar alarm-clock-temperature-process nil)

(defun alarm-clock-get-temperature ()
  (when (and alarm-clock-temperature-process
	     (process-live-p alarm-clock-temperature-process))
    (delete-process alarm-clock-temperature-process))
  (setq alarm-clock-temperature-process
	(make-process
	 :name "get-temperature"
	 :buffer (generate-new-buffer " *temperature*")
	 :command (list "snmpget"
			"-v1" "-O" "v"
			"-c" "public"
			"80.91.231.4" ".1.3.6.1.4.1.21239.5.1.4.1.5.2")
	 :sentinel
	 (lambda (proc _status)
	   (unless (process-live-p proc)
	     (with-current-buffer (process-buffer proc)
	       (goto-char (point-min))
	       (when (re-search-forward "INTEGER: \\([0-9]+\\)" nil t)
		 (setq alarm-clock-temperature
		       (/ (string-to-number (match-string 1)) 10.0)))
	       (kill-buffer (current-buffer))))))))

(defvar alarm-clock-sleeve-process nil)
(defvar alarm-clock-sleeve nil)
  
(defun alarm-clock-get-sleeve ()
  (when alarm-clock-sleeve-process
    (delete-process alarm-clock-sleeve-process))
  (with-current-buffer (generate-new-buffer " *sleeve*")
    (set-buffer-multibyte nil)
    (setq alarm-clock-sleeve-process
	  (make-process
	   :name "get-sleeve"
	   :buffer (current-buffer)
	   :command (list "curl" "--output" "-"
			  "http://rocket-sam/smalldisplay/sleeve720.png")
	   :sentinel
	   (lambda (proc _status)
	     (unless (process-live-p proc)
	       (with-current-buffer (process-buffer proc)
		 (setq alarm-clock-sleeve (buffer-string))
		 (kill-buffer (current-buffer)))))))))

(defvar-keymap alarm-clock-mode-map
  "d" #'alarm-clock-key
  "h" #'alarm-clock-key
  "l" #'alarm-clock-key

  "c" #'alarm-clock-key
  "g" #'alarm-clock-key
  "k" #'alarm-clock-key

  "b" #'alarm-clock-key
  "f" #'alarm-clock-key
  "j" #'alarm-clock-key

  "e" #'alarm-clock-key

  "i" #'alarm-clock-set-alarm
  
  "a" #'alarm-clock-cancel-alarm

  "4" #'alarm-clock-menu-down
  "6" #'alarm-clock-menu-up
  "5" #'alarm-clock-menu-execute

  "1" #'alarm-clock-decrease-volume
  "3" #'alarm-clock-increase-volume
  "2" #'alarm-clock-pause)

(defvar alarm-clock-key-sequence "")

(defun alarm-clock-key ()
  "Record a key for the alarm."
  (interactive)
  (let ((map '( ?d "7"
		?h "8"
		?l "9"
	            
		?c "4"
		?g "5"
		?k "6"
	            
		?b "1"
		?f "2"
		?j "3"
	            
		?e "0")))
    (if (not alarm-clock-stop-alarm)
	;; If the alarm is sounding, hitting any key will cancel it.
	(alarm-clock-cancel-alarm)
      (setq alarm-clock-key-sequence    
	    (if-let ((digit (plist-get map last-command-event)))
		(concat alarm-clock-key-sequence digit)
	      ""))
      (alarm-clock-message alarm-clock-key-sequence))))

(defun alarm-clock-mode ()
  (interactive)
  (setq major-mode 'alarm-clock-mode)
  (setq mode-name "Alarm-Clock")
  (use-local-map alarm-clock-mode-map)
  (setq mode-line-buffer-identification
	'("Alarm-Clock"))
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun setup-alarm-clock ()
  (server-start)
  (switch-to-buffer (set-buffer (get-buffer-create "*alarm-clock*")))
  (setq mode-line-format nil)
  (erase-buffer)
  (alarm-clock-mode)
  (set-face-background 'fringe "black")
  (setq default-frame-alist
	(nconc (list '(mouse-color . "black")
		     '(cursor-type . box)
		     '(cursor-color . "black"))
	       default-frame-alist))
  (blink-cursor-mode -1)
  (start-alarm-clock)
  (alarm-clock-reposition)
  (alarm-clock-reposition 10))

(defun alarm-clock-pause ()
  (interactive)
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-pause))
  (alarm-clock-message "Pause/Play"))

(defvar alarm-clock-volume 1)

(defun alarm-clock-decrease-volume ()
  (interactive)
  (setq alarm-clock-volume (max (- alarm-clock-volume 0.1) 0))
  (alarm-clock-message (format "Volume: %.1f" alarm-clock-volume))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,alarm-clock-volume "bedroom")))

(defun alarm-clock-increase-volume ()
  (interactive)
  (setq alarm-clock-volume (min (+ alarm-clock-volume 0.1) 9))
  (alarm-clock-message (format "Volume: %.1f" alarm-clock-volume))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,alarm-clock-volume "bedroom")))

(defvar alarm-clock-alarm nil)
(defvar alarm-clock-message nil)
(defvar alarm-clock-previous-key-sequence nil)

(defun alarm-clock-set-alarm ()
  (interactive)
  (let ((time alarm-clock-key-sequence)
	(input alarm-clock-key-sequence))
    (setq time
	  (cond
	   ((= (length time) 1)
	    (format "0%s:00" time))
	   ((= (length time) 2)
	    (format "%s:00" time))
	   ((= (length time) 3)
	    (format "0%s:%s" (substring time 0 1) (substring time 1)))
	   ((= (length time) 4)
	    (format "%s:%s" (substring time 0 2) (substring time 2)))
	   (t "")))
    (alarm-clock-cancel-alarm)
    (setq alarm-clock-key-sequence "")
    ;; Check whether the alarm clock is a valid time.
    (let ((bits (mapcar #'string-to-number (split-string time ":"))))
      (unless (<= 0 (car bits) 23)
	(setq time ""))
      (unless (<= 0 (car bits) 59)
	(setq time "")))
    (setq alarm-clock-alarm-time time)
    (if (equal time "")
	(when (cl-plusp (length input))
	  (alarm-clock-message (format "Invalid: %S" input)))
      (let ((when (alarm-clock-number-of-seconds-until time)))
	(setq alarm-clock-alarm 
	      (run-at-time when nil #'alarm-clock-sound-alarm)
	      alarm-clock-previous-key-sequence input)
	(alarm-clock-message
	 (format "(...en %s)"
		 (cond
		  ((< when 60)
		   (format "%ds" when))
		  ((< when 3600)
		   (format "%dm" (/ when 60)))
		  (t
		   (format "%dh et %dm"
			   (/ when 3600)
			   (/ (% when 3600) 60))))))))))

(defun alarm-clock-number-of-seconds-until (alarm-clock)
  (let ((seconds 0)
	(now (time-to-seconds (current-time))))
    (while (not (string= alarm-clock (format-time-string "%H:%M"
							(seconds-to-time (+ seconds now)))))
      (cl-incf seconds 40))
    (while (string= alarm-clock (format-time-string "%H:%M"
						   (seconds-to-time (+ seconds now))))
      (cl-decf seconds))
    seconds))

(defun alarm-clock-emacsclient (command)
  (call-process "emacsclient" nil nil nil
		"--server-file=rocket-sam" 
		"--eval" command))

(defvar alarm-clock-alarm-process nil)
(defvar alarm-clock-stop-alarm t)

(defun alarm-clock-sound-alarm ()
  (let ((volume 10)
	func)
    (setq alarm-clock-stop-alarm nil
	  alarm-clock-alarm-count 0)
    (setq func
	  (lambda ()
	    (call-process "amixer" nil nil nil
			  "-c" "2" "sset" "PCM"
			  (format "%d%%" volume))
	    (cl-incf volume 10)
	    (when (> volume 100)
	      (setq volume 50))
	    (setq alarm-clock-alarm-process
		  (make-process
		   :name "alarm"
		   :buffer (get-buffer-create " *alarm*")
		   :command (list "mpg123" "-o" "alsa" "-a" "plughw:2,0"
				  (expand-file-name
				   "alarm.mp3"
				   (file-name-directory
				    (find-library-name "alarm-clock"))))
		   :sentinel
		   (lambda (proc _status)
		     (unless (process-live-p proc)
		       (kill-buffer (process-buffer proc))
		       (when (and (not alarm-clock-stop-alarm)
				  ;; Stop after running for two minutes.
				  (< alarm-clock-alarm-count 120))
			 (funcall func))))))))
    (funcall func)))
  
(defun alarm-clock-cancel-alarm ()
  (interactive)
  (setq alarm-clock-alarm-time "")
  (setq alarm-clock-stop-alarm t)
  (when alarm-clock-alarm-process
    (delete-process alarm-clock-alarm-process))
  (alarm-clock-message "Cancelled alarm")
  (ignore-errors
    (cancel-timer alarm-clock-alarm)))

(defvar alarm-clock-dim 0)

(defun alarm-clock-make-svg (time alarm temperature sleeve width height)
  (let ((svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height
		   :fill "#000000")
    (when sleeve
      (svg-embed svg sleeve "image/png" t
		 :x 0
		 :y 0
		 :width width
		 :height height
		 :opacity 0.4))
    (svg-text svg time
	      :x (/ width 2.0)
	      :y 440
	      :text-anchor "middle"
	      :font-size 200
	      :font-weight "bold"
	      :fill "white"
    	      :font-family "futura")
    (svg-text svg temperature
	      :x (/ width 2.0)
	      :y (- height 50)
	      :font-size 100
	      :text-anchor "middle"
	      :font-weight "bold"
	      :fill "#888"
    	      :font-family "futura")
    (svg-text svg alarm
	      :x (/ width 2.0)
	      :y 120
	      :font-size 100
	      :text-anchor "middle"
	      :font-weight "bold"
	      :fill (if alarm-clock-stop-alarm
			(car alarm-clock-alarm-colors)
		      (elt alarm-clock-alarm-colors
			   (mod (cl-incf alarm-clock-alarm-count) 2)))
    	      :font-family "futura")
    (when alarm-clock-message
      (svg-text svg (cdr alarm-clock-message)
		:x (- (/ width 2.0) 210)
		:y 500
		:font-size 80
		:text-anchor "middle"
		:font-weight "bold"
		:transform "rotate(-30)"
		:fill "red"
    		:font-family "futura")
      (setcar alarm-clock-message (1- (car alarm-clock-message)))
      (when (< (car alarm-clock-message) 0)
	(setq alarm-clock-message nil)))
    (when (> alarm-clock-dim 0)
      (svg-rectangle svg 0 0 width height
		     :fill "#000000"
		     :opacity alarm-clock-dim))
    (when (> alarm-clock-menu-item -1)
      (let* ((margin 40)
	     (stride (/ (- height (* margin 2)) (length alarm-clock-menu))))
	(svg-rectangle svg
		       margin (+ margin (* alarm-clock-menu-item stride))
		       (- width (* margin 2))
		       stride
		       :fill "yellow")
	(svg-rectangle svg margin margin
		       (- width (* margin 2)) (- height (* margin 2))
		       :fill "none"
		       :stroke-color "red"
		       :stroke-width 8)
	(cl-loop for (text _) in alarm-clock-menu
		 for y from (+ margin 20) 
		 upto (- height (* margin 2))
		 by stride
		 do
		 (svg-text svg text
			   :x (+ margin 20)
			   :y (+ y 70)
			   :font-size 80
			   :font-weight "bold"
			   :fill "red"
			   :font-family "futura")
		 (svg-line svg
			   margin (+ y 110)
			   (- width margin) (+ y 110)
			   :stroke-width 4
			   :stroke-color "red"))))
    (insert-image (svg-image svg :width 720))
    (goto-char (point-min))))

(defun alarm-clock-light-on ()
  "Turn the light for the alarm alarm-clock monitor on."
  (interactive)
  (alarm-clock-message "Lights on")
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/") exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room room-bedroom on))))

(defun alarm-clock-light-off ()
  "Turn the light for the alarm alarm-clock monitor on."
  (interactive)
  (alarm-clock-message "Lights off")
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/")
			 exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room room-bedroom off))))

(defun alarm-clock-message (string)
  (setq alarm-clock-message (cons 5 string))
  (alarm-clock-display))

(defvar alarm-clock-sensor-process nil)

(defun alarm-clock-start-sensor ()
  (when alarm-clock-sensor-process
    (delete-process alarm-clock-sensor-process))
  (with-current-buffer (get-buffer-create " *yocto*")
    (setq alarm-clock-sensor-process
	  (make-process
	   :name "yocto"
	   :buffer (current-buffer)
	   :command (list (expand-file-name
			   "yocto_light.py"
			   (file-name-directory
			    (find-library-name "alarm-clock"))))
	   :filter (lambda (proc string)
		     (with-current-buffer (process-buffer proc)
		       (goto-char (point-max))
		       (insert string)
		       (when (and (bolp)
				  (re-search-backward " \\([.0-9]+\\) +lx" nil t))
			 (let ((lx (string-to-number (match-string 1))))
			   (erase-buffer)
			   (alarm-clock-adjust-brightness lx)))))))))

(defun alarm-clock-adjust-brightness (lx)
  (setq alarm-clock-dim
	(if (> lx 16)
	    0
	  (- 0.7 (/ lx 32.0))))
  ;;(message "lx: %s dim: %s" lx alarm-clock-dim)
  )

(defun alarm-clock-reposition (&optional timeout)
  (interactive)
  (run-at-time
   (or 5 timeout) nil
   (lambda ()
     (let ((frame-resize-pixelwise t))
       (set-frame-position (selected-frame) 0 0)
       (set-frame-width (selected-frame) 820 nil t)
       (set-frame-height (selected-frame) 820 nil t)))))

(defun alarm-clock-night-night ()
  (if (not alarm-clock-previous-key-sequence)
      (alarm-clock-message "No previous alarm")
    (setq alarm-clock-key-sequence alarm-clock-previous-key-sequence)
    (alarm-clock-set-alarm))
  ;; Turn the lights in the flat off.
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/")
			 exec-path)))
    (eval-at-async "lights" "rocket-sam" 8705
		   '(jukebox-pause-and-flat-off))))

(defun alarm-clock-flat-off ()
  (alarm-clock-message "Flat off")
  ;; Turn the lights in the flat off.
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/")
			 exec-path)))
    (eval-at-async "lights" "rocket-sam" 8705
		   '(jukebox-flat-off))))

(defun alarm-clock-flat-off-not-office ()
  (alarm-clock-message "Flat off (not office)")
  ;; Turn the lights in the flat off.
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/")
			 exec-path)))
    (eval-at-async "lights" "rocket-sam" 8705
		   '(jukebox-flat-off-not-office))))

(defvar alarm-clock-network-failures 0)

(defun alarm-clock-check-network ()
  (make-process
   :name "ping"
   :buffer nil
   :command (list "ping" "-c" "1" "-W" "1" "192.168.1.46")
   :sentinel 
   (lambda (proc event)
     (when (string-prefix-p "finished" event)
       (if (zerop (process-exit-status proc))
	   (setq alarm-clock-network-failures 0)
	 ;; Network has been down for a minute.
	 (when (> (cl-incf alarm-clock-network-failures) 6)
	   (start-process "nmcli" nil "nmcli" "con" "up" "Beige")
	   (setq alarm-clock-network-failures 0)))))))

(defvar alarm-clock-menu
  '(("Nighty Nighty" alarm-clock-night-night)
    ("Lights On" alarm-clock-light-on)
    ("Lights Off" alarm-clock-light-off)
    ("Flat Off" alarm-clock-flat-off)
    ("Flat Off/Office" alarm-clock-flat-off-not-office)))

(defvar alarm-clock-menu-item -1)

(defun alarm-clock-menu-down ()
  "Go down in the menu."
  (interactive)
  (setq alarm-clock-menu-item
	(min (1+ alarm-clock-menu-item) (1- (length alarm-clock-menu))))
  (alarm-clock-display))

(defun alarm-clock-menu-up ()
  "Go up in the menu."
  (interactive)
  (setq alarm-clock-menu-item
	(max (1- alarm-clock-menu-item) -1))
  (alarm-clock-display))

(defun alarm-clock-menu-execute ()
  "Execute the selected menu item."
  (interactive)
  (let ((func (cadr (elt alarm-clock-menu alarm-clock-menu-item))))
    (when func
      (funcall func))
    (setq alarm-clock-menu-item -1)
    (alarm-clock-display)))

(provide 'alarm-clock)

;;; alarm-clock.el ends here
