;;; powerline.el --- Rewrite of Powerline

;; Copyright (c) 2011 Donald Ephraim Curtis

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline
;; Version: 2.0
;; Keywords: mode-line

;;; Code:

(defvar powerline-buffer-size-suffix t)

(defface powerline-active1 '((t (:background "grey22" :inherit mode-line))) "Powerline face 1."
  :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :inherit mode-line))) "Powerline face 2."
  :group 'powerline)

(defface powerline-inactive1 '((t (:background "grey11" :inherit mode-line-inactive))) "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2 '((t (:background "grey20" :inherit mode-line-inactive))) "Powerline face 2."
  :group 'powerline)


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize FUNC.
If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of FUNC."
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


(defun pl/arrow-row-right (dots width &optional left-pad right-pad)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (unless left-pad (setq left-pad 0))
  (unless right-pad (setq right-pad 0))
  (concat "\""
          (make-string left-pad ?.)
          (make-string dots ?.)
          (make-string (- width dots) ? )
          (make-string right-pad ? )
          "\","))

(defun pl/arrow-row-left (dots width &optional left-pad right-pad)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (unless left-pad (setq left-pad 0))
  (unless right-pad (setq right-pad 0))
  (concat "\""
          (make-string left-pad ?.)
          (make-string (- width dots) ?.)
          (make-string dots ? )
          (make-string right-pad ? )
          "\","))


(defun pl/size-up (unitsize width)
  "Extend WIDTH to the nearest multiple of the UNITSIZE."
  (* unitsize (ceiling width unitsize)))

(defmacro pl/arrow-xpm (dir)
  "Generate an arrow xpm function for DIR."
  (let ((rowfunc (intern (format "pl/arrow-row-%s" (symbol-name dir)))))
    `(defun ,(intern (format "pl/arrow-xpm-%s" (symbol-name dir))) (height unitwidth color1 color2)
       (unless unitwidth (setq unitwidth (frame-char-width)))
       (let* ((dots (/ height 2))
              (width (ceiling height 2))
              (total-width (pl/size-up unitwidth width))
              (pad (- total-width width))
              (left-pad (/ pad 2))
              (right-pad (/ pad 2))
              (odd (not (= dots width))))
         (if (eq ',dir 'left) (setq left-pad (1+ left-pad))
           (setq right-pad (1+ right-pad)))
         (create-image
          (concat
           (format "/* XPM */
static char * arrow_%s[] = {
\"%s %s 2 1\",
\". c %s\",
\"  c %s\",
" (symbol-name ',dir) total-width height (or color1 "None") (or color2 "None"))
           (mapconcat (lambda (d) (,rowfunc d width left-pad right-pad)) (number-sequence 1 dots) "\n")
           (and odd "\n")
           (and odd (,rowfunc (+ dots 1) width left-pad right-pad))
           "\n"
           (mapconcat (lambda (d) (,rowfunc d width left-pad right-pad)) (number-sequence dots 1 -1) "\n")
           "};")
          'xpm t :ascent 'center)))))

(memoize (pl/arrow-xpm left))
(memoize (pl/arrow-xpm right))


(defun pl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 for enabled and COLOR2 for disabled bits specified in DATA."
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
   'xpm t :ascent 'center))

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
                (pl/percent-xpm (frame-char-height) pmax pmin we ws (* (frame-char-width) width) color1 color2))))


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
(defmacro defpltext (name body)
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
  (propertize  (concat
                (when (and str (eq pad 'l)) " ")
                str
                (when (and str (eq pad 'r)) " "))
               'face face))


;;;###autoload
(defun powerline-fill (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (eq 'right (get-scroll-bar-mode))
    (setq reserve (+ reserve 3)))
  (propertize " " 'display `((space :align-to (- right-fringe ,reserve))) 'face face))

(defun powerline-fill-center (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (eq 'right (get-scroll-bar-mode))
    (setq reserve (+ reserve 3)))
  (propertize " " 'display `((space :align-to (- center ,reserve))) 'face face))


;;;###autoload
(defpltext powerline-major-mode
  (propertize mode-name
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

;;;###autoload
(defpltext powerline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                        (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                        (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                        (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))

                                        map)))
             (split-string (format-mode-line minor-mode-alist)) " "))


;;;###autoload
(defpltext powerline-narrow
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
(defpltext powerline-vc
  (when (and (buffer-file-name (current-buffer))
             vc-mode)
    (format-mode-line '(vc-mode vc-mode))))


;;;###autoload
(defpltext powerline-buffer-size
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
(defpltext powerline-buffer-id
  (format-mode-line mode-line-buffer-identification))

;;;###autoload
(defmacro defplsep (name docstring &optional func)
  "Create a function NAME with optional DOCSTRING that takes arguments FACE1, FACE2 and call FUNC with the background colors for those faces or \"None\"."
  (unless func
    (setq func docstring)
    (setq docstring nil))
  `(defun ,name
     (&optional face1 face2)
     ,docstring
     (let* ((color1 (if face1 (face-attribute face1 :background) "None"))
            (color2 (if face2 (face-attribute face2 :background) "None"))
            (image (,func (frame-char-height)  (frame-char-width) color1 color2)))
       (propertize (make-string (ceiling (car (image-size image))) ? )
                   'display image))))


;;;###autoload
(defplsep powerline-arrow-left pl/arrow-xpm-left)

;;;###autoload
(defplsep powerline-arrow-right pl/arrow-xpm-right)

;;;###autoload
(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)))
                        (face1 (if active 'powerline-active1 'powerline-inactive1))
                        (face2 (if active 'powerline-active2 'powerline-inactive2))
                        (lhs (concat
                              (powerline-raw "%*" nil 'l)
                              (powerline-buffer-size nil 'l)
                              (powerline-buffer-id nil 'l)

                              (powerline-arrow-right nil face1)

                              (powerline-major-mode face1 'l)
                              (powerline-minor-modes face1 'l)
                              (powerline-raw mode-line-process face1 'l)

                              (powerline-narrow face1 'l)

                              (powerline-arrow-right face1 face2)

                              (powerline-vc face2)
                              ))
                        (rhs (concat
                              (powerline-raw global-mode-string face2 'r)

                              (powerline-arrow-left face2 face1)

                              (powerline-raw "%4l" face1 'r)
                              (powerline-raw ":" face1)
                              (powerline-raw "%3c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%6p" nil 'r)

                              (powerline-hud face2 face1))))
                   (concat lhs (powerline-fill face2 (length (format-mode-line rhs))) rhs)))))


(provide 'powerline)


;;; powerline.el ends here
