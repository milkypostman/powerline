powerline
=========

Emacs version of the Vim powerline.

This is a proposed version 2.0 of the original [Emacs Powerline](http://www.emacswiki.org/emacs/PowerLine) which is a fork of [vim-powerline](https://github.com/Lokaltog/vim-powerline).  


## Installation

    (require 'powerline)
    (powerline-default)
    
The `powerline-default` function provides a *default* mode-line.  You can write your own or copy and modify the `mode-line-format` contents provided.

    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (eq (frame-selected-window) (selected-window)))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (lhs (list
                                  (powerline-raw "%*" nil 'l)
                                  (powerline-buffer-size nil 'l)
                                  (powerline-buffer-id nil 'l)
  
                                  (powerline-raw " ")
                                  (powerline-arrow-right nil face1)
  
                                  (powerline-major-mode face1 'l)
                                  (powerline-minor-modes face1 'l)
                                  (powerline-raw mode-line-process face1 'l)
  
                                  (powerline-narrow face1 'l)
  
                                  (powerline-arrow-right face1 face2)
  
                                  (powerline-vc face2)
                                  ))
                            (rhs (list
                                  (powerline-raw global-mode-string face2 'r)
  
                                  (powerline-arrow-left face2 face1)
  
                                  (powerline-raw "%4l" face1 'r)
                                  (powerline-raw ":" face1)
                                  (powerline-raw "%3c" face1 'r)
  
                                  (powerline-arrow-left face1 nil)
                                  (powerline-raw " ")
  
                                  (powerline-raw "%6p" nil 'r)
  
                                  (powerline-hud face2 face1))))
                       (concat
                        (powerline-render lhs)
                        (powerline-fill face2 (powerline-width rhs))
                        (powerline-render rhs))))))
                      
The last line of this is what actually puts it all together.  It makes sure that the XPM images are rendered properly.  But notice we pre-compile the `rhs` of the statusline and this allows us to more accurately right justify the text.  There are currently no other "separators", just the arrows.  I will be introducing more gradually.  This version should be easier to modify.


### Centering
                      
You can also setup a default mode-line using,

    (powerline-default-center)
    
which sets up a mode-line demonstrating the ability to *center* mode-line information.  It contains the following setup,

    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (eq (frame-selected-window) (selected-window)))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (lhs (list
                                  (powerline-raw "%*" nil 'l)
                                  (powerline-buffer-size nil 'l)
                                  (powerline-buffer-id nil 'l)
  
                                  (powerline-raw " ")
                                  (powerline-arrow-right nil face1)
  
                                  (powerline-raw mode-line-process face1 'l)
  
                                  (powerline-narrow face1 'l)
  
                                  (powerline-vc face1)
                                  ))
                            (rhs (list
                                  (powerline-raw global-mode-string face2 'r)
  
                                  (powerline-raw "%4l" face1 'r)
                                  (powerline-raw ":" face1)
                                  (powerline-raw "%3c" face1 'r)
  
                                  (powerline-arrow-left face1 nil)
                                  (powerline-raw " ")
                                  (powerline-raw "%6p" nil 'r)
                                  (powerline-hud face2 face1)))
                            (center (list
                                     (powerline-raw " " face1)
                                     (powerline-arrow-right face1 face2)
                                     (powerline-major-mode face2 'l)
                                     (powerline-raw " :" face2)
                                     (powerline-minor-modes face2 'l)
                                     (powerline-raw " " face2)
                                     (powerline-arrow-left face2 face1))))
  
                       (concat
                        (powerline-render lhs)
                        (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                        (powerline-render center)
                        (powerline-fill face1 (powerline-width rhs))
                        (powerline-render rhs))))))                       
  


## Improvements from this rewrite:

* Cleaner code.
* Try to simply be a *library* that provides functions for generating a mode-line
* Make right-aligned text actually be flush against the right side.
* Separators are designed to dynamically size their height based on the font settings.
* Separators spread their width to the nearest character width.  (This is required to make right-aligned text actually be right-aligned)


## Implementing New Separators

The function should return an XPM image created using the `create-image` function.

There is a function called `memoize` that will help make calling the function multiple times with the same parameters be much quicker by caching the return value.

Each divider should have the signature: `(face1 face2 &optional height)`

`face1` : the left-hand face

`face2` : the right-hand face

`height` : specifies the height of the XPM, most of time this is `(font-char-height)`

Separators should consider the `height` when they are created so that the mode-line can change sizes based on the font height.

