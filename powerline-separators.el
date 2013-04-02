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

(defmacro pl/arrow (dir)
  "Generate an arrow xpm function for DIR."
  (let ((start (if (eq dir 'right) 'width 0))
        (end (if (eq dir 'right) '(- width midwidth) 'midwidth))
        (incr (if (eq dir 'right) -1 1)))
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
            :face (when (and face1 face2) (if (eq ',dir 'right) face1 face2))))))))

(defun pl/interpolate (color1 color2)
  "Interpolate between two hex colors, they must be supplied as hex colors with leading #"
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
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@#          \",
\"@@@         \",
\"@@@@        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@#      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@#     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@    \",
\"@@@@@@@@    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@#  \",
\"@@@@@@@@@@@#\"};"
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
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          #@\",
\"         @@@\",
\"        @@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"      #@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     #@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"    #@@@@@@@\",
\"    @@@@@@@@\",
\"    @@@@@@@@\",
\"   #@@@@@@@@\",
\"   @@@@@@@@@\",
\"  @@@@@@@@@@\",
\"#@@@@@@@@@@@\"};"
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



(defun pl/roundstub-left (face1 face2)
  "Return an XPM roundstub left."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * roundstub_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@          \",
\"@@@@        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@#       \",
\"@@@@        \",
\"@@          \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/roundstub-right (face1 face2)
  "Return an XPM roundstub right."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * roundstub_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          @@\",
\"        @@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       #@@@@\",
\"        @@@@\",
\"          @@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))

(defun pl/zigzag-left (face1 face2)
  "Return an XPM zigzag left."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * zigzag_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \"};"
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
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))

(defun pl/butt-left (face1 face2)
  "Return an XPM butt left."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * butt_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/butt-right (face1 face2)
  "Return an XPM butt right."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * butt_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))

(defun pl/chamfer (face1 face2)
  "Return an XPM chamfer string representing."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * chamfer[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@@    \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))


(defun pl/rounded (face1 face2)
  "Return an XPM rounded string representing."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * rounded[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@#        \",
\"@@@@@#      \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))


(defun pl/contour-left (face1 face2)
  "Return an XPM contour-left string representing."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * contour_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@           \",
\"@@#         \",
\"@@@#        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@#      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@#     \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@@@ \"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/contour-right (face1 face2)
  "Return an XPM contour-right string representing."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * contour_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"           @\",
\"         #@@\",
\"        #@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"      #@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     #@@@@@@\",
\"     @@@@@@@\",
\"    #@@@@@@@\",
\"   #@@@@@@@@\",
\" @@@@@@@@@@@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))


(defun pl/slant-left (face1 face2)
  "Return an XPM left slant string representing."
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * slant_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@         \",
\"@@@@         \",
\"@@@@@        \",
\"@@@@@        \",
\"@@@@@@       \",
\"@@@@@@       \",
\"@@@@@@@      \",
\"@@@@@@@      \",
\"@@@@@@@@     \",
\"@@@@@@@@     \",
\"@@@@@@@@@    \",
\"@@@@@@@@@    \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@@ \",
\"@@@@@@@@@@@@\"};"
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None"))
     'xpm t :ascent 'center)))

(defun pl/slant-right (face1 face2)
  "Return an XPM right slant string representing@"
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None")))
    (create-image
     (format "/* XPM */
static char * slant_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"        @@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"    @@@@@@@@\",
\"    @@@@@@@@\",
\"   @@@@@@@@@\",
\"   @@@@@@@@@\",
\"  @@@@@@@@@@\",
\"  @@@@@@@@@@\",
\" @@@@@@@@@@@\",
\" @@@@@@@@@@@\",
\"@@@@@@@@@@@@\",
\"@@@@@@@@@@@@\"};"
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None"))
     'xpm t :ascent 'center)))


(defun pl/curve-left (face1 face2 &optional height)
  "Return an XPM left curve string representing@"
  (unless height (setq height (max (frame-char-height) 16)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 12) 0)))
    (create-image
     (format "/* XPM */
static char * curve_left[] = {
\"5 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"     \",
\"@#   \",
\"@@@  \",
\"@@@# \",
\"@@@@ \",
\"@@@@#\",
%s
\"@@@@#\",
\"@@@@ \",
\"@@@# \",
\"@@@  \",
\"@#   \",
\"     \"};"
             (+ 12 fill-height)
             (if color1 color1 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color2 color2 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@\",")))
     'xpm t :ascent 'center :face face2)))


(defun pl/curve-right (face1 face2 &optional height)
  "Return an XPM right curve string representing@"
  (unless height (setq height (max (frame-char-height) 16)))
  (let ((color1 (if face1 (pl/hex-color (face-attribute face1 :background)) "None"))
        (color2 (if face2 (pl/hex-color (face-attribute face2 :background)) "None"))
        (fill-height (max (- height 12) 0)))
    (create-image
     (format "/* XPM */
static char * curve_right[] = {
\"5 %s 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"     \",
\"   #@\",
\"  @@@\",
\" #@@@\",
\" @@@@\",
\"#@@@@\",
%s
\"#@@@@\",
\" @@@@\",
\" #@@@\",
\"  @@@\",
\"   #@\",
\"     \"};"
             (+ 12 fill-height)
             (if color2 color2 "None")
             (if (and face1 face2) (pl/interpolate color2 color1) "None")
             (if color1 color1 "None")
             (apply 'concat (loop repeat fill-height collect "\"@@@@@\",")))
     'xpm t :ascent 'center :face face2)))



(provide 'powerline-separators)

;;; powerline-separators.el ends here
