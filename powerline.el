;;; powerline.el --- fancy statusline

;; Name: Emacs Powerline
;; Author: Unknown
;; Version: 1.1
;; Keywords: statusline

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;;; Changelog:

;; v1.0 - Unknown posted to wiki
;; v1.1 - Guard clause around the powerline output, so that if
;;        powerline tries to output something unexpected, it won't
;;        just fail and flail-barf.  (JonathanArkell)

;;; Code:

(defvar powerline-color1)
(defvar powerline-color2)

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(set-face-attribute 'mode-line nil
                    :background "OliveDrab3"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(scroll-bar-mode -1)

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-right-xpm
  (color1 color2)
  "Return an XPM right curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"         ...\",
\"         ...\",
\"       .....\",
\"       .....\",
\"       .....\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"       .....\",
\"       .....\",
\"       .....\",
\"         ...\",
\"         ...\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"...         \",
\"...         \",
\".....       \",
\".....       \",
\".....       \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\".....       \",
\".....       \",
\".....       \",
\"...         \",
\"...         \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun half-xpm
  (color1 color2)
  (make-xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun percent-xpm
  (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                   0
                 (round (* 17 (/ (float ws) (float pmax))))))
         (fe   (if (eq pmax we)
                   17
                 (round (* 17 (/ (float we) (float pmax))))))
         (o    nil)
         (i    0))
    (while (< i 18)
      (setq o (cons
               (if (and (<= fs i)
                        (<= i fe))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
	(val-sym (gensym))
	(args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
	 (if ,val-sym
	     ,val-sym
	   (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)
(memoize 'half-xpm)
(memoize 'percent-xpm)



(defvar powerline-minor-modes nil)
(defvar powerline-arrow-shape 'arrow)
(defun powerline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                            :foreground "white"
                            :background bg
                            :box nil))
        cface)
    nil))
(defun powerline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (powerline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (cond ((eq powerline-arrow-shape 'arrow)
                            (arrow-left-xpm color1 color2))
                           ((eq powerline-arrow-shape 'curve)
                            (curve-left-xpm color1 color2))
                           ((eq powerline-arrow-shape 'half)
                            (half-xpm color2 color1))
                           (t
                            (arrow-left-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape
                                                  (cond ((eq powerline-arrow-shape 'arrow) 'curve)
                                                        ((eq powerline-arrow-shape 'curve) 'half)
                                                        ((eq powerline-arrow-shape 'half)  'arrow)
                                                        (t                                 'arrow)))
                                            (redraw-modeline))))
       ""))))
(defun powerline-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (powerline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
       (propertize " " 'display
                   (cond ((eq powerline-arrow-shape 'arrow)
                          (arrow-right-xpm color1 color2))
                         ((eq powerline-arrow-shape 'curve)
                          (curve-right-xpm color1 color2))
                         ((eq powerline-arrow-shape 'half)
                          (half-xpm color2 color1))
                         (t
                          (arrow-right-xpm color1 color2)))
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive)
                                          (setq powerline-arrow-shape
                                                (cond ((eq powerline-arrow-shape 'arrow) 'curve)
                                                      ((eq powerline-arrow-shape 'curve) 'half)
                                                      ((eq powerline-arrow-shape 'half)  'arrow)
                                                      (t                                 'arrow)))
                                          (redraw-modeline))))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))
(defun powerline-make-fill
  (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (powerline-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
        (propertize " " 'display '((space :align-to (- right-fringe 21)))
                    'face plface)
      (propertize " " 'display '((space :align-to (- right-fringe 24)))
                  'face plface))))
(defun powerline-make-text
  (string color &optional fg localmap)
  (let ((plface (powerline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))
(defun powerline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (powerline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (powerline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (powerline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (powerline-make-right  string color1 color1 localmap))
        (t                             (powerline-make-text   string color1 localmap))))

(defmacro defpowerline (name string)
  `(defun ,(intern (concat "powerline-" (symbol-name name)))
     (side color1 &optional color2)
     (powerline-make side
                     (let ((result ,string))
					   (if (not (or (stringp result)
									(null result)))
						   (progn
							 (message "Powerline got %S but expected a string" result)
							 " ERR")
						   result))
                     color1 color2)))

(defun powerline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                    nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defpowerline arrow       "")
(defpowerline buffer-id   (propertize (car (propertized-buffer-identification "%12b"))
                                      'face (powerline-make-face color1)))
(defvar powerline-buffer-size-suffix t)
(defpowerline buffer-size (propertize
                            (if powerline-buffer-size-suffix
                                "%I"
                              "%i")
                            'local-map (make-mode-line-mouse-map
                                        'mouse-1 (lambda () (interactive)
                                                   (setq powerline-buffer-size-suffix
                                                         (not powerline-buffer-size-suffix))
                                                   (redraw-modeline)))))
(defpowerline rmw         "%*")
(defpowerline major-mode  (propertize mode-name
                                      'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line down-mouse-1]
                                                     `(menu-item ,(purecopy "Menu Bar") ignore
                                                                 :filter (lambda (_) (mouse-menu-major-mode-map))))
                                                   (define-key map [mode-line mouse-2] 'describe-mode)
                                                   (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                                   map)))
(defpowerline process      mode-line-process)
(defpowerline minor-modes (let ((mms (split-string (format-mode-line minor-mode-alist))))
                            (apply 'concat
                                   (mapcar #'(lambda (mm)
                                              (propertize (if (string= (car mms)
                                                                       mm)
                                                              mm
                                                            (concat " " mm))
                                                          'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                                                          'local-map (let ((map (make-sparse-keymap)))
                                                                       (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                                                       (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                                                       (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                                                       (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                                                                       map)))
                                           mms))))
(defpowerline row         "%4l")
(defpowerline column      "%3c")
(defpowerline percent     "%6p")
(defpowerline narrow      (let (real-point-min real-point-max)
                            (save-excursion
                              (save-restriction
                                (widen)
                                (setq real-point-min (point-min) real-point-max (point-max))))
                            (when (or (/= real-point-min (point-min))
                                      (/= real-point-max (point-max)))
                              (propertize "Narrow"
                                          'help-echo "mouse-1: Remove narrowing from the current buffer"
                                          'local-map (make-mode-line-mouse-map
                                                      'mouse-1 'mode-line-widen)))))
(defpowerline status      "%s")
(defpowerline global      global-mode-string)
(defpowerline emacsclient mode-line-client)

(defpowerline vc          (when (and (buffer-file-name (current-buffer))
                                     vc-mode)
							(symbol-name (vc-mode-line (buffer-file-name (current-buffer) )))))

(defpowerline percent-xpm (propertize "  "
                                      'display
                                      (let (pmax
                                            pmin
                                            (ws (window-start))
                                            (we (window-end)))
                                        (save-restriction
                                          (widen)
                                          (setq pmax (point-max))
                                          (setq pmin (point-min)))
                                        (percent-xpm pmax pmin we ws 15 color1 color2))))

(setq-default mode-line-format
              (list "%e"
                    '(:eval (concat
                             (powerline-rmw            'left   nil  )
                             (powerline-buffer-size    'left   nil  )
                             (powerline-buffer-id      'left   nil  powerline-color1  )
                             (powerline-major-mode     'left        powerline-color1  )
                             (powerline-process        'text        powerline-color1  )
                             (powerline-minor-modes    'left        powerline-color1  )
                             (powerline-narrow         'left        powerline-color1  powerline-color2  )
                             (powerline-global         'center                        powerline-color2  )
                             (powerline-vc             'center                        powerline-color2  )
                             (powerline-make-fill                                     powerline-color2  )
                             (powerline-row            'right       powerline-color1  powerline-color2  )
                             (powerline-make-text      ":"          powerline-color1  )
                             (powerline-column         'right       powerline-color1  )
                             (powerline-percent        'right  nil  powerline-color1  )
                             (powerline-percent-xpm    'text   nil  powerline-color1  )
                             (powerline-make-text      "  "    nil  )))))




(provide 'powerline)

;;; powerline.el ends here
