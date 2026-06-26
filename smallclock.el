;;; smallclock.el -- a small Emacs alarm clock -*- lexical-binding: t -*-

;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: home automation

;; This file is not part of GNU Emacs.

;; smallclock.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; smallclock.el is distributed in the hope that it will be useful,
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

(defvar smallclock-temperature nil)
(defvar smallclock-alarm-time "")

(defun smallclock-display ()
  (with-current-buffer (get-buffer-create "*smallclock*")
    (erase-buffer)
    (smallclock-make-svg (format-time-string "%H:%M")
			 smallclock-alarm-time
			 (if smallclock-temperature
			     (format "%.1f°" smallclock-temperature)
			   "no temp")
			 smallclock-sleeve
			 720 720)
    (put-text-property (point-min) (point-max) 'keymap nil)))

(defvar smallclock-timer nil)
(defvar smallclock-temperature-timer nil)
(defvar smallclock-sleeve-timer nil)

(defun start-smallclock ()
  (when smallclock-timer
    (cancel-timer smallclock-timer))
  (setq smallclock-timer (run-at-time 1 1 #'smallclock-display))
  (when smallclock-temperature-timer
    (cancel-timer smallclock-temperature-timer))
  (setq smallclock-temperature-timer
	(run-at-time 2 30 #'smallclock-get-temperature))
  (when smallclock-sleeve-timer
    (cancel-timer smallclock-sleeve-timer))
  (setq smallclock-sleeve-timer
	(run-at-time 3 30 #'smallclock-get-sleeve)))

(defvar smallclock-poll-process nil)

(defun smallclock-get-temperature ()
  (when smallclock-poll-process
    (delete-process smallclock-poll-process))
  (setq smallclock-poll-process
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
		 (setq smallclock-temperature
		       (/ (string-to-number (match-string 1)) 10.0)))
	       (kill-buffer (current-buffer))))))))

(defvar smallclock-sleeve-process nil)
(defvar smallclock-sleeve nil)
  
(defun smallclock-get-sleeve ()
  (when smallclock-poll-process
    (delete-process smallclock-poll-process))
  (with-current-buffer (generate-new-buffer " *sleeve*")
    (set-buffer-multibyte nil)
    (setq smallclock-poll-process
	  (make-process
	   :name "get-sleeve"
	   :buffer (current-buffer)
	   :command (list "curl" "--output" "-"
			  "http://rocket-sam/smalldisplay/sleeve720.png")
	   :sentinel
	   (lambda (proc _status)
	     (unless (process-live-p proc)
	       (with-current-buffer (process-buffer proc)
		 (setq smallclock-sleeve (buffer-string))
		 (kill-buffer (current-buffer)))))))))

(defvar-keymap smallclock-mode-map
  "d" #'smallclock-key
  "h" #'smallclock-key
  "l" #'smallclock-key

  "c" #'smallclock-key
  "g" #'smallclock-key
  "k" #'smallclock-key

  "b" #'smallclock-key
  "f" #'smallclock-key
  "j" #'smallclock-key

  "e" #'smallclock-key

  "i" #'smallclock-set-alarm
  
  "a" #'smallclock-cancel-alarm
  "4" #'smallclock-light-off
  "6" #'smallclock-light-on

  "1" #'smallclock-decrease-volume
  "3" #'smallclock-increase-volume
  "2" #'smallclock-pause)

(defvar smallclock-key-sequence "")

(defun smallclock-key ()
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
    (setq smallclock-key-sequence    
	  (if-let ((digit (plist-get map last-command-event)))
	      (concat smallclock-key-sequence digit)
	    ""))))

(defun smallclock-mode ()
  (interactive)
  (setq major-mode 'smallclock-mode)
  (setq mode-name "Smallclock")
  (use-local-map smallclock-mode-map)
  (setq mode-line-buffer-identification
	'("Smallclock"))
  (setq truncate-lines t)
  (buffer-disable-undo)
  (set-face-background 'fringe "black")
  (setq default-frame-alist
	(nconc (list '(mouse-color . "black")
		     '(cursor-type . box)
		     '(cursor-color . "black"))
	       default-frame-alist))
  (blink-cursor-mode -1))

(defun setup-smallclock ()
  (server-start)
  (switch-to-buffer (set-buffer (get-buffer-create "*smallclock*")))
  (setq mode-line-format nil)
  (erase-buffer)
  (smallclock-mode)
  (start-smallclock))

(defun smallclock-pause ()
  (interactive)
  (smallclock-emacsclient "(jukebox-pause)"))

(defvar smallclock-volume 2)

(defun smallclock-decrease-volume ()
  (interactive)
  (setq smallclock-volume (max (- smallclock-volume 0.1) 0))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,smallclock-volume "bedroom")))

(defun smallclock-increase-volume ()
  (interactive)
  (setq smallclock-volume (min (+ smallclock-volume 0.1) 9))
  (eval-at "lights" "rocket-sam" 8705
	   `(jukebox-set-vol-volume ,smallclock-volume "bedroom")))

(defvar smallclock-alarm nil)

(defun smallclock-set-alarm ()
  (interactive)
  (let ((time smallclock-key-sequence))
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
    (smallclock-cancel-alarm)
    (setq smallclock-key-sequence "")
    ;; Check whether the alarm clock is a valid time.
    (let ((bits (mapcar #'string-to-number (split-string time ":"))))
      (unless (<= 0 (car bits) 23)
	(setq time ""))
      (unless (<= 0 (car bits) 59)
	(setq time "")))
    (setq smallclock-alarm-time time)
    (when nil
      (setq smallclock-alarm 
	    (run-at-time (smallclock-number-of-seconds-until time)
			 nil #'smallclock-sound-alarm)))
    (smallclock-display)))

(defun smallclock-number-of-seconds-until (smallclock)
  (let ((seconds 0)
	(now (time-to-seconds (current-time))))
    (while (not (string= smallclock (format-time-string "%H:%M"
						   (seconds-to-time (+ seconds now)))))
      (cl-incf seconds 40))
    (while (string= smallclock (format-time-string "%H:%M"
					      (seconds-to-time (+ seconds now))))
      (cl-decf seconds))
    seconds))

(defun smallclock-emacsclient (command)
  (call-process "emacsclient" nil nil nil
		"--server-file=rocket-sam" 
		"--eval" command))

(defvar smallcontrol-alarm-process nil)
(defvar smallcontrol-stop-alarm t)

(defun smallclock-sound-alarm ()
  (let ((volume 10)
	func)
    (setq smallcontrol-stop-alarm nil)
    (setq func
	  (lambda ()
	    (call-process "amixer" nil nil nil
			  "-c" "1" "sset" "PCM"
			  (format "%d%%" volume))
	    (cl-incf volume 10)
	    (when (> volume 100)
	      (setq volume 50))
	    (setq smallcontrol-alarm-process
		  (make-process
		   :name "alarm"
		   :buffer (get-buffer-create " *alarm*")
		   :command (list "mpg123" "-o" "alsa" "-a" "plughw:1,0"
				  (expand-file-name
				   "alarm.mp3"
				   (file-name-directory
				    (find-library-name "smallclock"))))
		   :sentinel
		   (lambda (proc _status)
		     (unless (process-live-p proc)
		       (kill-buffer (process-buffer proc))
		       (unless smallcontrol-stop-alarm
			 (funcall func))))))))
    (funcall func)))
  
(defun smallclock-cancel-alarm ()
  (interactive)
  (setq smallclock-alarm-time "")
  (setq smallcontrol-stop-alarm t)
  (when smallcontrol-alarm-process
    (delete-process smallcontrol-alarm-process))
  (smallclock-display)
  (ignore-errors
    (cancel-timer smallclock-alarm)))

(defun smallclock-make-svg (time alarm temperature sleeve width height)
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
	      :fill "#888"
    	      :font-family "futura")
    (insert-image (svg-image svg :width 720))
    (goto-char (point-min))))

(defun smallclock-light-on ()
  "Turn the light for the alarm smallclock monitor on."
  (interactive)
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/") exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room bedroom on))))

(defun smallclock-light-off ()
  "Turn the light for the alarm smallclock monitor on."
  (interactive)
  (let ((exec-path (cons (expand-file-name "~/src/eval-server.el/") exec-path)))
    (eval-at-async "lights" "rocket-sam" 8701
		   '(tellstick-switch-room bedroom off))))

(defvar smallclock-sensor-process nil)

(defun smallclock-start-sensor ()
  (with-current-buffer (get-buffer-create " *yocto*")
    (setq smallclock-sensor-process
	  (make-process
	   :name "yocto"
	   :buffer (current-buffer)
	   :command (list (expand-file-name
			   "yocto_light.py"
			   (file-name-directory
			    (find-library-name "smallclock"))))
	   :filter (lambda (proc string)
		     (with-current-buffer (process-buffer proc)
		       (goto-char (point-max))
		       (insert string)
		       (when (and (bolp)
				  (re-search-backward " \\([.0-9]\\) +lx" nil t))
			 (let ((lx (string-to-number (match-string 1))))
			   (erase-buffer)
			   (smallclock-adjust-brightness lx)))))))))

(defun smallclock-adjust-brightness (lx)
  )

(provide 'smallclock)

;;; smallclock.el ends here
