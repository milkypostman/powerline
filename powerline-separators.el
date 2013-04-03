;;; powerline-separators.el --- Separators for Powerline

;; Copyright (c) 2013,2012 Donald Ephraim Curtis
;; Copyright (c) 2013 Jason Milkins
;; Copyright (c) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline
;; Version: 2.0
;; Keywords: mode-line

;;; Code:

(require 'cl)

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
  (let ((start (if (eq dir 'right) 'width 0))
        (end (if (eq dir 'right) '(- width midwidth) 'midwidth))
        (incr (if (eq dir 'right) -1 1)))
    `(defun ,(intern (format "powerline-arrow-%s" (symbol-name dir)))
       (face1 face2 &optional height)
       (when window-system
         (unless height (setq height (pl/separator-height)))
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
            :face (when (and face1 face2) (if (eq ',dir 'right) face1 face2))))))))

(defun pl/interpolate (color1 color2)
  "Interpolate between COLOR1 and COLOR2.

COLOR1 and COLOR2 must be supplied as hex strings with leading #."
  (let* (
         (c1 (replace-regexp-in-string "#" "" color1))
         (c2 (replace-regexp-in-string "#" "" color2))
         (c1r (string-to-number (substring c1 0 2) 16)) (c1b (string-to-number (substring c1 2 4) 16)) (c1g (string-to-number (substring c1 4 6) 16))
         (c2r (string-to-number (substring c2 0 2) 16)) (c2b (string-to-number (substring c2 2 4) 16)) (c2g (string-to-number (substring c2 4 6) 16))
         (red (/ (+ c1r c2r) 2)) (grn (/ (+ c1g c2g) 2)) (blu (/ (+ c1b c2b) 2)))

    (format "#%02X%02X%02X" red grn blu))
  )

(defun pl/wave-left (face1 face2)
  "Return an XPM wave left from FACE1 to FACE2."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * wave_left[] = {
\"11 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"#          \",
\"@@         \",
\"@@@        \",
\"@@@#       \",
\"@@@@       \",
\"@@@@#      \",
\"@@@@@      \",
\"@@@@@      \",
\"@@@@@#     \",
\"@@@@@@     \",
\"@@@@@@     \",
\"@@@@@@#    \",
\"@@@@@@@    \",
\"@@@@@@@    \",
\"@@@@@@@#   \",
\"@@@@@@@@   \",
\"@@@@@@@@#  \",
\"@@@@@@@@@@#\"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color1 color2) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/wave-right (face1 face2)
  "Return an XPM wave right from FACE1 to FACE2."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * wave_right[] = {
\"11 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          #\",
\"         @@\",
\"        @@@\",
\"       #@@@\",
\"       @@@@\",
\"      #@@@@\",
\"      @@@@@\",
\"      @@@@@\",
\"     #@@@@@\",
\"     @@@@@@\",
\"     @@@@@@\",
\"    #@@@@@@\",
\"    @@@@@@@\",
\"    @@@@@@@\",
\"   #@@@@@@@\",
\"   @@@@@@@@\",
\"  @@@@@@@@@\",
\"#@@@@@@@@@@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))


(defun pl/brace-left (face1 face2)
  "Return an XPM brace left."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * brace_left[] = {
\"4 19 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"    \",
\"#   \",
\"@   \",
\"@   \",
\"@   \",
\"@   \",
\"@#  \",
\"@#  \",
\"@@# \",
\"@@@@\",
\"@@# \",
\"@#  \",
\"@#  \",
\"@   \",
\"@   \",
\"@   \",
\"@   \",
\"#   \",
\"    \"
};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/brace-right (face1 face2)
  "Return an XPM brace right."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * brace_right[] = {
\"4 19 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"    \",
\"   #\",
\"   @\",
\"   @\",
\"   @\",
\"   @\",
\"  #@\",
\"  #@\",
\" #@@\",
\"@@@@\",
\" #@@\",
\"  #@\",
\"  #@\",
\"   @\",
\"   @\",
\"   @\",
\"   @\",
\"   #\",
\"    \"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))



(defun pl/roundstub-left (face1 face2 &optional height)
  "Return an XPM roundstub left."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * roundstub_left[] = {
\"3 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\"@@ \",
\"@@#\",
%s
\"@@#\",
\"@@ \",
\"   \"};"
             height
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@\",")))
     'xpm t :ascent 'center :face face1)))

(defun pl/roundstub-right (face1 face2 &optional height)
  "Return an XPM roundstub right."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * roundstub_right[] = {
\"3 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\" @@\",
\"#@@\",
%s
\"#@@\",
\" @@\",
\"   \"};"
             height
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@\",")))
     'xpm t :ascent 'center :face face2)))

(defun pl/zigzag-left (face1 face2)
  "Return an XPM zigzag left."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * zigzag_left[] = {
\"3 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\"@  \",
\"@@ \",
\"@@@\",
\"@@ \",
\"@  \",
\"   \",
\"@  \",
\"@@ \",
\"@@@\",
\"@@ \",
\"@  \",
\"   \",
\"@  \",
\"@@ \",
\"@@@\",
\"@@ \",
\"@  \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/zigzag-right (face1 face2)
  "Return an XPM zigzag right."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * zigzag_right[] = {
\"3 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\"  @\",
\" @@\",
\"@@@\",
\" @@\",
\"  @\",
\"   \",
\"  @\",
\" @@\",
\"@@@\",
\" @@\",
\"  @\",
\"   \",
\"  @\",
\" @@\",
\"@@@\",
\" @@\",
\"  @\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))

(defun pl/butt-left (face1 face2 &optional height)
  "Return an XPM butt left."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * butt_left[] = {
\"3 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\"@  \",
\"@@ \",
%s
\"@@ \",
\"@  \",
\"   \"};"
             height
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@\",")))
     'xpm t :ascent 'center :face face1)))

(defun pl/butt-right (face1 face2 &optional height)
  "Return an XPM butt right."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * butt_right[] = {
\"3 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"   \",
\"  @\",
\" @@\",
%s
\" @@\",
\"  @\",
\"   \"};"
             height
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@\",")))
     'xpm t :ascent 'center :face face2)))

(defmacro pl/chamfer (dir)
  "Generate an arrow xpm function for DIR."
  (let ((start (if (eq dir 'right) 'width 0))
        (end (if (eq dir 'right) 0 'width))
        (incr (if (eq dir 'right) -1 1)))
    `(defun ,(intern (format "powerline-chamfer-%s" (symbol-name dir)))
       (face1 face2 &optional height)
       (when window-system
         (unless height (setq height (pl/separator-height)))
         (let* ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
                (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
                (width 3)
                (fill-height (- height width 1))
                (seq (number-sequence ,start ,end ,incr)))
           (message "%s" (+ ,incr ,end))
           (create-image
            (concat
             (format "/* XPM */
static char * arrow_%s[] = {
\"%s %s 2 1\",
\"@ c %s\",
\"  c %s\",
" (symbol-name ',dir) 3 height (or color1 "None") (or color2 "None"))
             (mapconcat
              (lambda (d) (pl/xpm-row-string d width ?@ ? )) seq "\n")
             (apply 'concat (loop repeat fill-height collect (pl/xpm-row-string ,end width ?@ ? )))
             "};")
            'xpm t :ascent 'center
            :face (when (and face1 face2) (if (eq ',dir 'right) face1 face2))))))))



(defun pl/rounded-left (face1 face2 &optional height)
  "Return an XPM rounded string representing."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * rounded[] = {
\"6 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"#     \",
\"@@#   \",
\"@@@@  \",
\"@@@@# \",
\"@@@@@ \",
\"@@@@@#\",
%s
};"
             height
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@@\",")))
     'xpm t :ascent 'center)))

(defun pl/rounded-right (face1 face2 &optional height)
  "Return an XPM rounded string representing."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 6) 0)))
    (create-image
     (format "/* XPM */
static char * rounded[] = {
\"6 %s 3 1\",
\"  c %s\",
\"# c %s\",
\"@ c %s\",
\"     #\",
\"   #@@\",
\"  @@@@\",
\" #@@@@\",
\" @@@@@\",
\"#@@@@@\",
%s
};"
             height
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@@\",")))
     'xpm t :ascent 'center)))


(defun pl/contour-left (face1 face2 &optional height)
  "Return an XPM contour-left string representing."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 11) 0)))
    (create-image
     (format "/* XPM */
static char * contour_left[] = {
\"10 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          \",
\"@#        \",
\"@@#       \",
\"@@@#      \",
\"@@@@      \",
\"@@@@#     \",
%s
\"@@@@@#    \",
\"@@@@@@    \",
\"@@@@@@#   \",
\"@@@@@@@#  \",
\"@@@@@@@@@@\"};"
             height
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@     \",")))
     'xpm t :ascent 'center :face face1)))

(defun pl/contour-right (face1 face2 &optional height)
  "Return an XPM contour-right string representing."
  (unless height (setq height (pl/separator-height)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 11) 0)))
    (create-image
     (format "/* XPM */
static char * contour_right[] = {
\"10 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          \",
\"        #@\",
\"       #@@\",
\"      #@@@\",
\"      @@@@\",
\"     #@@@@\",
%s
\"    #@@@@@\",
\"    @@@@@@\",
\"   #@@@@@@\",
\"  #@@@@@@@\",
\"@@@@@@@@@@\"};"
             height
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None")
             (apply 'concat (loop repeat fill-height collect "\"     @@@@@\",")))
     'xpm t :ascent 'center :face face2)))

(defmacro pl/slant (dir)
  "Generate an arrow xpm function for DIR."
  (let ((start (if (eq dir 'right) 'width 0))
        (end (if (eq dir 'right) 0 'width))
        (incr (if (eq dir 'right) -1 1)))
    `(defun ,(intern (format "powerline-slant-%s" (symbol-name dir)))
       (face1 face2 &optional height)
       (when window-system
         (unless height (setq height (pl/separator-height)))
         (let* ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
                (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
                (rows (ceiling height 2))
                (width (1- (ceiling height 2)))
                (seq (number-sequence ,start ,end ,incr)))
           (create-image
            (concat
             (format "/* XPM */
static char * arrow_%s[] = {
\"%s %s 2 1\",
\"@ c %s\",
\"  c %s\",
" (symbol-name ',dir) width (* rows 2) (or color1 "None") (or color2 "None"))
             (mapconcat
              (lambda (d) (concat (pl/xpm-row-string d width ?@ ? )
                                  (pl/xpm-row-string d width ?@ ? ))) seq "\n")
             "};")
            'xpm t :ascent 'center
            :face (when (and face1 face2) (if (eq ',dir 'right) face1 face2))))))))



(defun pl/curve-left (face1 face2 &optional height)
  "Return an XPM left curve string representing@"
  (unless height (setq height (max (pl/separator-height) 14)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 12) 0)))
    (create-image
     (format "/* XPM */
static char * curve_left[] = {
\"4 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"    \",
\"#   \",
\"@@  \",
\"@@# \",
\"@@@ \",
\"@@@#\",
%s
\"@@@#\",
\"@@@ \",
\"@@# \",
\"@@  \",
\"#   \",
\"    \"};"
             (+ 12 fill-height)
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@\",")))
     'xpm t :ascent 'center :face face2)))


(defun pl/curve-right (face1 face2 &optional height)
  "Return an XPM right curve string representing@"
  (unless height (setq height (max (pl/separator-height) 14)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 12) 0)))
    (create-image
     (format "/* XPM */
static char * curve_right[] = {
\"4 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"    \",
\"   #\",
\"  @@\",
\" #@@\",
\" @@@\",
\"#@@@\",
%s
\"#@@@\",
\" @@@\",
\" #@@\",
\"  @@\",
\"   #\",
\"    \"};"
             (+ 12 fill-height)
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@\",")))
     'xpm t :ascent 'center :face face2)))



(provide 'powerline-separators)

;;; powerline-separators.el ends here
