;;; powerline-themes.el --- Themes for Powerline

;; Copyright (C) 2012-2015 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.0
;; Keywords: mode-line

;;; Commentary:
;;
;; Themes for Powerline.
;; Included themes: default, center, center-evil, vim, and nano.
;;

;;; Code:

(defcustom powerline-display-buffer-size t
  "When non-nil, display the buffer size."
  :type 'boolean)

(defcustom powerline-display-mule-info t
  "When non-nil, display the mule info."
  :type 'boolean)

(defcustom powerline-display-hud t
  "When non-nil, display the hud."
  :type 'boolean)

;;;###autoload
(defun powerline-default-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     (powerline-hud face2 face1)))
			  (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (boundp 'erc-modified-channels-object)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw " :" face2)
					(powerline-minor-modes face2 'l)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

(defun powerline-center-evil-theme ()
  "Setup a mode-line with major, evil, and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     (powerline-hud face2 face1)))
			  (center (append (list (powerline-raw " " face1)
						(funcall separator-left face1 face2)
						(when (boundp 'erc-modified-channels-object)
						  (powerline-raw erc-modified-channels-object face2 'l))
						(powerline-major-mode face2 'l)
						(powerline-process face2)
						(powerline-raw " " face2))
					  (if (split-string (format-mode-line minor-mode-alist))
					      (append (if evil-mode
							  (list (funcall separator-right face2 face1)
								(powerline-raw evil-mode-line-tag face1 'l)
								(powerline-raw " " face1)
								(funcall separator-left face1 face2)))
						      (list (powerline-minor-modes face2 'l)
							    (powerline-raw " " face2)
							    (funcall separator-right face2 face1)))
					    (list (powerline-raw evil-mode-line-tag face2)
						  (funcall separator-right face2 face1))))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-vim-theme ()
  "Setup a Vim-like mode-line."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
				     (powerline-raw "[" mode-line 'l)
				     (powerline-major-mode mode-line)
				     (powerline-process mode-line)
				     (powerline-raw "]" mode-line)
				     (when (buffer-modified-p)
				       (powerline-raw "[+]" mode-line))
				     (when buffer-read-only
				       (powerline-raw "[RO]" mode-line))
				     (powerline-raw "[%z]" mode-line)
				     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
				     (when (and (boundp 'which-func-mode) which-func-mode)
				       (powerline-raw which-func-format nil 'l))
				     (when (boundp 'erc-modified-channels-object)
				       (powerline-raw erc-modified-channels-object face1 'l))
				     (powerline-raw "[" mode-line 'l)
				     (powerline-minor-modes mode-line)
				     (powerline-raw "%n" mode-line)
				     (powerline-raw "]" mode-line)
				     (when (and vc-mode buffer-file-name)
				       (let ((backend (vc-backend buffer-file-name)))
					 (when backend
					   (concat (powerline-raw "[" mode-line 'l)
						   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
						   (powerline-raw "]" mode-line)))))))
			  (rhs (list (powerline-raw '(10 "%i"))
				     (powerline-raw global-mode-string mode-line 'r)
				     (powerline-raw "%l," mode-line 'l)
				     (powerline-raw (format-mode-line '(10 "%c")))
				     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
		     (concat (powerline-render lhs)
			     (powerline-fill mode-line (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-nano-theme ()
  "Setup a nano-like mode-line."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (lhs (list (powerline-raw (concat "GNU Emacs "
							    (number-to-string
							     emacs-major-version)
							    "."
							    (number-to-string
							     emacs-minor-version))
						    nil 'l)))
			  (rhs (list (if (buffer-modified-p) (powerline-raw "Modified" nil 'r))))
			  (center (list (powerline-raw "%b" nil))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center nil (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill nil (powerline-width rhs))
			     (powerline-render rhs)))))))

(defface powerline-modified-face
  '((((class color))
     (:background "#FFA335" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify modified files."
  :group 'powerline)

(defface powerline-normal-face
  '((((class color))
     (:background "#4F9D03" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify unchanged files."
  :group 'powerline)

(defface powerline-default-dictionary-active-face
  '((((class color))
     (:background "#8A2BE2" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify default dictionary in the active buffer."
  :group 'powerline)

(defface powerline-default-dictionary-inactive-face
  '((((class color))
     (:background "thistle" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify default dictionary in inactive buffers."
  :group 'powerline)

(defface powerline-other-dictionary-active-face
  '((((class color))
     (:background "yellow" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify another dictionary in the active buffer."
  :group 'powerline)

(defface powerline-other-dictionary-inactive-face
  '((((class color))
     (:background "LightYellow1" :foreground "black" :weight bold))
    (t (:weight bold)))
  "Face to fontify another dictionary in inactive buffers."
  :group 'powerline)

;;;###autoload
(defun powerline-leuven-theme ()
  "Setup the leuven mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active
                                         'mode-line
                                       'mode-line-inactive))
                          (face1 (if active
                                     'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active
                                     'powerline-active2
                                   'powerline-inactive2))
                          (default-dictionary-face
                            (if active
                                'powerline-default-dictionary-active-face
                              'powerline-default-dictionary-inactive-face))
                          (other-dictionary-face
                           (if active
                               'powerline-other-dictionary-active-face
                             'powerline-other-dictionary-inactive-face))
                          (separator-left
                           (intern
                            (format "powerline-%s-%s"
                                    powerline-default-separator
                                    (car powerline-default-separator-dir))))
                          (separator-right
                           (intern
                            (format "powerline-%s-%s"
                                    powerline-default-separator
                                    (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; vc mode
                                (when (and (fboundp 'vc-switches)
                                           buffer-file-name
                                           vc-mode)
                                  (if (eq (vc-state buffer-file-name) 'up-to-date)
                                      (powerline-vc 'powerline-normal-face 'r)
                                    (powerline-vc 'powerline-modified-face 'r)))

                                (when (and (not (fboundp 'vc-switches))
                                           buffer-file-name
                                           vc-mode)
                                  (powerline-vc face1 'r))

                                (when (and buffer-file-name
                                           vc-mode)
                                  (if (eq (vc-state buffer-file-name) 'up-to-date)
                                      (funcall separator-left 'powerline-normal-face mode-line)
                                    (funcall separator-left 'powerline-modified-face mode-line)))

                                ;; "modified" indicator
                                (if (not (buffer-modified-p))
                                    (powerline-raw "%*" nil 'l)
                                  (powerline-raw "%*" 'mode-line-emphasis 'l))

                                (powerline-raw mode-line-mule-info nil 'l)

                                (powerline-buffer-id nil 'l)

                                (when (and (boundp 'which-func-mode) which-func-mode)
                                  (powerline-raw which-func-format nil 'l))

                                (powerline-raw " ")
                                (funcall separator-left mode-line face1)
                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                (powerline-minor-modes face2 'l)
                                (powerline-narrow face2 'l)
                                (powerline-raw " " face2)
                                (funcall separator-left face2 mode-line)))
                          (rhs (list (powerline-raw global-mode-string mode-line 'r)
                                     (funcall separator-right mode-line face1)

                                     (powerline-raw "%l" face1 'l)
                                     (powerline-raw ", " face1 'l)
                                     (powerline-raw "%c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%4p" nil 'r)
                                     (funcall separator-right mode-line face2)
                                     (powerline-buffer-size face2 'l)
                                     (powerline-raw " " face2)

                                     (let ((dict (and (featurep 'ispell)
                                                      (or
                                                       ispell-local-dictionary
                                                       ispell-dictionary))))
                                       ;; Add 2 spaces after the language indicator
                                       ;; (for GNU/Linux).
                                       (cond (buffer-read-only
                                              (powerline-raw "%%%%  " default-dictionary-face 'l))
                                             ((null dict)
                                              (powerline-raw "--  " default-dictionary-face 'l))
                                             (t
                                              (powerline-raw (concat (substring dict 0 2) "  ") other-dictionary-face 'l))))

                                     ;; (powerline-hud face2 face1)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'powerline-themes)

;;; powerline-themes.el ends here
