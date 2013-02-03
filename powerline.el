;;; powerline.el --- Rewrite of Powerline

;; Copyright (c) 2012 Donald Ephraim Curtis
;; Copyright (c) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline
;; Version: 2.0
;; Keywords: mode-line

;;; Code:

(require 'cl)

(defface powerline-active1 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(defcustom powerline-buffer-size-suffix t
  "Display the buffer size suffix."
  :group 'powerline
  :type 'bool)

(defun create-or-get-powerline-cache ()
  "Return a frame-local hash table that acts as a memoization
cache for powerline. Create one if the frame doesn't have one
yet."
  (or (frame-parameter nil 'powerline-cache)
      (let ((table (make-hash-table :test 'equal)))
        ;; Store it as a frame-local variable
        (modify-frame-parameters nil `((powerline-cache . ,table)))
        table)))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the memoized function over
the original function.  Use frame-local memoization."
  (typecase func
    (symbol (fset func (memoize-wrap-frame-local (symbol-function func))) func)
    (function (memoize-wrap-frame-local func))))

(defun memoize-wrap-frame-local (func)
  "Return the memoized version of FUNC.  The memoization cache is
frame-local."
  (let ((cache-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let* ((,cache-sym (create-or-get-powerline-cache))
              (,val-sym (gethash ,args-sym ,cache-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,cache-sym))))))


(defun pl/xpm-row-string (width total left right)
  "Generate a string of two types of characters filled on the
left by WIDTH out of TOTAL. "
  (concat "\"" (make-string width left)
          (make-string (- total width) right)
          "\","))

(defun pl/hex-color (color)
  "Gets the hexadecimal value of a color"
  (let ((ret color))
    (cond
     ((string= "#" (substring color 0 1))
      (setq ret (upcase ret)))
     ((color-defined-p color)
      (setq ret (concat "#"
                        (mapconcat
                         (lambda(val)
                           (format "%02X" (* val 255)))
                         (color-name-to-rgb color) ""))))
     (t (setq ret nil)))
    (symbol-value 'ret)))

(defmacro pl/arrow (dir)
  "Generate an arrow xpm function for DIR."
  (let ((start (if (eq dir 'left) 'width 0))
        (end (if (eq dir 'left) '(- width midwidth) 'midwidth))
        (incr (if (eq dir 'left) -1 1)))
    `(defun ,(intern (format "powerline-arrow-%s" (symbol-name dir)))
       (face1 face2 &optional height)
       (when window-system
         (unless height (setq height (frame-char-height)))
         (let* ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
                (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
                (midwidth (1- (/ height 2)))
                (width (1- (ceiling height 2)))
                (seq (number-sequence ,start ,end ,incr))
                (odd (not (= midwidth width))))
           (create-image
            (concat
             (format "/* XPM */
static char * arrow_%s[] = {
\"%s %s 2 1\",
\". c %s\",
\"  c %s\",
" (symbol-name ',dir) width height (or color1 "None") (or color2 "None"))
             (mapconcat
              (lambda (d) (pl/xpm-row-string d width ?. ? )) seq "\n")
             (and odd (concat "\n" (pl/xpm-row-string (+ ,end ,incr) width ?. ? )))
             "\n"
             (mapconcat
              (lambda (d) (pl/xpm-row-string d width ?. ? )) (reverse seq) "\n")
             "};")
            'xpm t :ascent 'center
            :face (when (and face1 face2) (if (eq ',dir 'left) face1 face2))))))))

(defun powerline-reset ()
  "Reset memoized functions."
  (interactive)
  (memoize (pl/arrow left))
  (memoize (pl/arrow right)))
(powerline-reset)

(defun pl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 for enabled and COLOR2 for disabled bits specified in DATA."
  (when window-system
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
              (or color1 "None")
              (or color2 "None"))
      (let ((len  (length data))
            (idx  0))
        (apply 'concat
               (mapcar '(lambda (dl)
                          (setq idx (+ idx 1))
                          (concat
                           "\""
                           (concat
                            (mapcar '(lambda (d)
                                       (if (eq d 0)
                                           (string-to-char " ")
                                         (string-to-char ".")))
                                    dl))
                           (if (eq idx len)
                               "\"};"
                             "\",\n")))
                       data))))
     'xpm t :ascent 'center)))

(defun pl/percent-xpm
  (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm."
  (let* ((height- (1- height))
         (fillstart (round (* height- (/ (float winstart) (float pmax)))))
         (fillend (round (* height- (/ (float winend) (float pmax)))))
         (data nil)
         (i 0))
    (while (< i height)
      (setq data (cons
                  (if (and (<= fillstart i)
                           (<= i fillend))
                      (append (make-list width 1))
                    (append (make-list width 0)))
                  data))
      (setq i (+ i 1)))
    (pl/make-xpm "percent" color1 color2 (reverse data))))

(memoize 'pl/percent-xpm)

;;;###autoload
(defun powerline-hud (face1 face2 &optional width)
  (unless width (setq width 2))
  (let ((color1 (if face1 (face-attribute face1 :background) "None"))
        (color2 (if face2 (face-attribute face2 :background) "None"))
        pmax
        pmin
        (ws (window-start))
        (we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (propertize (make-string width ? )
                'display
                (pl/percent-xpm (frame-char-height) pmax pmin we ws
                                (* (frame-char-width) width) color1 color2))))


;;;###autoload
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


;;;###autoload
(defun powerline-concat (&rest strings)
  "Concatonate STRINGS and pad sides by spaces."
  (concat
   " "
   (mapconcat 'identity (delq nil strings) " ")
   " "))

;;;###autoload
(defmacro defpowerline (name body)
  `(defun ,name
     (&optional face pad)
     (let ((str ,body))
       (propertize (concat
                    (when (and str (eq pad 'l)) " ")
                    str
                    (when (and str (eq pad 'r)) " "))
                   'face face))))

;;;###autoload
(defun powerline-raw (str &optional face pad)
  (let ((rendered-str (format-mode-line str)))
    (propertize  (concat
                  (when (and rendered-str (eq pad 'l)) " ")
                  (if (listp str) rendered-str str)
                  (when (and rendered-str (eq pad 'r)) " "))
                 'face face)))


;;;###autoload
(defun powerline-fill (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun powerline-fill-center (face reserve)
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))


;;;###autoload
(defpowerline powerline-major-mode
  (propertize (format-mode-line mode-name)
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

;;;###autoload
(defpowerline powerline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map
                                          [mode-line down-mouse-1]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [mode-line mouse-2]
                                          (powerline-mouse 'minor 'help mm))
                                        (define-key map
                                          [mode-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [header-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        map)))
             (split-string (format-mode-line minor-mode-alist)) " "))


;;;###autoload
(defpowerline powerline-narrow
  (let (real-point-min real-point-max)
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

;;;###autoload
(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer))
             vc-mode)
    (format-mode-line '(vc-mode vc-mode))))


;;;###autoload
(defpowerline powerline-buffer-size
  (propertize
   (if powerline-buffer-size-suffix
       "%I"
     "%i")
   'local-map (make-mode-line-mouse-map
               'mouse-1 (lambda () (interactive)
                          (setq powerline-buffer-size-suffix
                                (not powerline-buffer-size-suffix))
                          (redraw-modeline)))))

;;;###autoload
(defpowerline powerline-buffer-id
  (format-mode-line mode-line-buffer-identification))

;;;###autoload
(defpowerline powerline-process
  (cond
   ((listp mode-line-process) (format-mode-line mode-line-process))
   (t mode-line-process)))

(defvar pl/minibuffer-selected-window-list '())

(defun pl/minibuffer-selected-window ()
  "Return the selected window when entereing the minibuffer."
  (when pl/minibuffer-selected-window-list
    (car pl/minibuffer-selected-window-list)))

(defun pl/minibuffer-setup ()
  "Save the minibuffer-selected-window to `pl/minibuffer-selected-window'."
  (push (minibuffer-selected-window) pl/minibuffer-selected-window-list))

(add-hook 'minibuffer-setup-hook 'pl/minibuffer-setup)

(defun pl/minibuffer-exit ()
  "Set `pl/minibuffer-selected-window' to nil."
  (pop pl/minibuffer-selected-window-list))

(add-hook 'minibuffer-exit-hook 'pl/minibuffer-exit)

(defun powerline-selected-window-active ()
  "Return whether the current window is active."
  (or (eq (frame-selected-window)
          (selected-window))
      (and (minibuffer-window-active-p
            (frame-selected-window))
           (eq (pl/minibuffer-selected-window)
               (selected-window)))))

;;;###autoload
(defun powerline-center-theme ()
  "Setup a default mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active 'powerline-active2
                                   'powerline-inactive2))
                          (lhs (list
                                (powerline-raw "%*" nil 'l)
                                (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)

                                (powerline-raw " ")
                                (powerline-arrow-right mode-line face1)

                                (powerline-narrow face1 'l)

                                (powerline-vc face1)))
                          (rhs (list
                                (powerline-raw global-mode-string face1 'r)

                                (powerline-raw "%4l" face1 'r)
                                (powerline-raw ":" face1)
                                (powerline-raw "%3c" face1 'r)

                                (powerline-arrow-left face1 mode-line)
                                (powerline-raw " ")
                                (powerline-raw "%6p" nil 'r)
                                (powerline-hud face2 face1)))
                          (center (list
                                   (powerline-raw " " face1)
                                   (powerline-arrow-right face1 face2)
                                   (when (boundp 'erc-modified-channels-object)
                                     (powerline-raw erc-modified-channels-object
                                                    face2 'l))
                                   (powerline-major-mode face2 'l)
                                   (powerline-process face2)
                                   (powerline-raw " :" face2)
                                   (powerline-minor-modes face2 'l)
                                   (powerline-raw " " face2)
                                   (powerline-arrow-left face2 face1))))

                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1 (/ (powerline-width center)
                                                      2.0))
                      (powerline-render center)
                      (powerline-fill face1 (powerline-width rhs))
                      (powerline-render rhs)))))))

;;;###autoload
(fset 'powerline-default-center 'powerline-center-theme)


;;;###autoload
(defun powerline-default-theme ()
  "Setup a default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active 'powerline-active2
                                   'powerline-inactive2))
                          (lhs (list
                                (powerline-raw "%*" nil 'l)
                                (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)

                                (powerline-raw " ")
                                (powerline-arrow-right mode-line face1)

                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object
                                                 face1 'l))

                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)

                                (powerline-raw " " face1)
                                (powerline-arrow-right face1 face2)

                                (powerline-vc face2)))
                          (rhs (list
                                (powerline-raw global-mode-string face2 'r)

                                (powerline-arrow-left face2 face1)

                                (powerline-raw "%4l" face1 'r)
                                (powerline-raw ":" face1)
                                (powerline-raw "%3c" face1 'r)

                                (powerline-arrow-left face1 mode-line)
                                (powerline-raw " ")

                                (powerline-raw "%6p" nil 'r)

                                (powerline-hud face2 face1))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))


;;;###autoload
(fset 'powerline-default 'powerline-default-theme)


;;;###autoload
(defun powerline-nano-theme ()
  "Setup a nano-like mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (lhs (list
                                (powerline-raw (concat
                                                "GNU Emacs "
                                                (number-to-string
                                                 emacs-major-version)
                                                "."
                                                (number-to-string
                                                 emacs-minor-version))
                                               nil 'l)))
                          (rhs (list
                                (if (buffer-modified-p)
                                    (powerline-raw "Modified" nil 'r))))
                          (center (list
                                   (powerline-raw "%b" nil))))

                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center nil (/ (powerline-width center)
                                                    2.0))
                      (powerline-render center)
                      (powerline-fill nil (powerline-width rhs))
                      (powerline-render rhs)))))))


(defun pl/render (item)
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (item item)))

(defun powerline-render (values)
  (mapconcat 'pl/render values ""))

(defun powerline-width (values)
  "Get the length of VALUES."
  (if values
      (let ((val (car values)))
        (+ (cond
            ((stringp val) (length (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (powerline-width (cdr values))))
    0))

(provide 'powerline)


;;; powerline.el ends here
