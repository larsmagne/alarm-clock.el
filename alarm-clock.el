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
	(run-at-time 3 30 #'alarm-clock-get-sleeve)))

(defvar alarm-clock-poll-process nil)

(defun alarm-clock-get-temperature ()
  (when alarm-clock-poll-process
    (delete-process alarm-clock-poll-process))
  (setq alarm-clock-poll-process
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
  (when alarm-clock-poll-process
    (delete-process alarm-clock-poll-process))
  (with-current-buffer (generate-new-buffer " *sleeve*")
    (set-buffer-multibyte nil)
    (setq alarm-clock-poll-process
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
  "4" #'alarm-clock-light-off
  "6" #'alarm-clock-light-on

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
    (setq alarm-clock-key-sequence    
	  (if-let ((digit (plist-get map last-command-event)))
	      (concat alarm-clock-key-sequence digit)
	    ""))))

(defun alarm-clock-mode ()
  (interactive)
  (setq major-mode 'alarm-clock-mode)
  (setq mode-name "Alarm-Clock")
  (use-local-map alarm-clock-mode-map)
  (setq mode-line-buffer-identification
	'("Alarm-Clock"))
  (setq truncate-lines t)
  (buffer-disable-undo)
  (set-face-background 'fringe "black")
  (setq default-frame-alist
	(nconc (list '(mouse-color . "black")
		     '(cursor-type . box)
		     '(cursor-color . "black"))
	       default-frame-alist))
  (blink-cursor-mode -1))

(defun setup-alarm-clock ()
  (server-start)
  (switch-to-buffer (set-buffer (get-buffer-create "*alarm-clock*")))
  (setq mode-line-format nil)
  (erase-buffer)
  (alarm-clock-mode)
  (start-alarm-clock))

(defun alarm-clock-pause ()
  (interactive)
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-pause))
  (setq alarm-clock-message
	(cons 5 (format "Pause/Play"))))

(defvar alarm-clock-volume 1)

(defun alarm-clock-decrease-volume ()
  (interactive)
  (setq alarm-clock-volume (max (- alarm-clock-volume 0.1) 0))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,alarm-clock-volume "bedroom"))
  (setq alarm-clock-message
	(cons 5 (format "Volume: %.1f" alarm-clock-volume))))

(defun alarm-clock-increase-volume ()
  (interactive)
  (setq alarm-clock-volume (min (+ alarm-clock-volume 0.1) 9))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,alarm-clock-volume "bedroom"))
  (setq alarm-clock-message
	(cons 5 (format "Volume: %.1f" alarm-clock-volume))))

(defvar alarm-clock-alarm nil)
(defvar alarm-clock-message nil)

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
	  (setq alarm-clock-message
		(cons 5 (format "Invalid: %S" input))))
      (let ((when (alarm-clock-number-of-seconds-until time)))
	(setq alarm-clock-alarm 
	      (run-at-time when nil #'alarm-clock-sound-alarm)
	      alarm-clock-message
	      (cons 5 (format "(...en %s)"
			      (cond
			       ((< when 60)
				(format "%ds" when))
			       ((< when 3600)
				(format "%dm" (/ when 60)))
			       (t
				(format "%dh et %dm"
					(/ when 3600)
					(/ (% when 3600) 60)))))))))
    (alarm-clock-display)))

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

(defvar smallcontrol-alarm-process nil)
(defvar smallcontrol-stop-alarm t)

(defun alarm-clock-sound-alarm ()
  (let ((volume 10)
	func)
    (setq smallcontrol-stop-alarm nil
	  alarm-clock-alarm-count 0)
    (setq func
	  (lambda ()
	    (call-process "amixer" nil nil nil
			  "-c" "2" "sset" "PCM"
			  (format "%d%%" volume))
	    (cl-incf volume 10)
	    (when (> volume 100)
	      (setq volume 50))
	    (setq smallcontrol-alarm-process
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
		       (when (and (not smallcontrol-stop-alarm)
				  ;; Stop after running for two minutes.
				  (< alarm-clock-alarm-count 120))
			 (funcall func))))))))
    (funcall func)))
  
(defun alarm-clock-cancel-alarm ()
  (interactive)
  (setq alarm-clock-alarm-time "")
  (setq smallcontrol-stop-alarm t)
  (when smallcontrol-alarm-process
    (delete-process smallcontrol-alarm-process))
  (setq alarm-clock-message (cons 5 "Cancelled alarm"))
  (alarm-clock-display)
  (ignore-errors
    (cancel-timer alarm-clock-alarm)))

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
	      :fill (if smallcontrol-stop-alarm
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
    (insert-image (svg-image svg :width 720))
    (goto-char (point-min))))

(defun alarm-clock-light-on ()
  "Turn the light for the alarm alarm-clock monitor on."
  (interactive)
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/") exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room bedroom on))))

(defun alarm-clock-light-off ()
  "Turn the light for the alarm alarm-clock monitor on."
  (interactive)
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/") exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room bedroom off))))

(defvar alarm-clock-sensor-process nil)

(defun alarm-clock-start-sensor ()
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
				  (re-search-backward " \\([.0-9]\\) +lx" nil t))
			 (let ((lx (string-to-number (match-string 1))))
			   (erase-buffer)
			   (alarm-clock-adjust-brightness lx)))))))))

(defun alarm-clock-adjust-brightness (_lx)
  )

(provide 'alarm-clock)

;;; alarm-clock.el ends here
