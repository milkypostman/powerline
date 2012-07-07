powerline
=========

Emacs version of the Vim powerline.


This is a proposed version 2.0 of the original [Emacs Powerline](http://www.emacswiki.org/emacs/PowerLine) which is a fork of [vim-powerline](https://github.com/Lokaltog/vim-powerline).


Improvements from this rewrite:

* Cleaner code.
* Try to simply be a *library* that provides functions for generating a mode-line
* Make right-aligned text actually be flush against the right side.
* Separators are designed to dynamically size their height based on the font settings.
* Separators spread their width to the nearest character width.  (This is required to make right-aligned text actually be right-aligned)

In essence, this extensions just provides functions that can be used in the `mode-line-format`.  The default version for powerline is:

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
    
                       
The last line of this is what actually puts it all together.  But notice we pre-compile the `rhs` of the statusline and this allows us to more accurately right justify the text.  There are currently no other "separators", just the arrows.  I will be introducing more gradually.  This version should be easier to modify.


## Implementing New Separators

**It's possible this API may change a bit in the future.**

The function should return an XPM image created using the `create-image` function.

There is a function called `memoize` that will help make calling the function multiple times with the same parameters be much quicker by caching the return value.

Each divider should have the signature: `(pl/<divider> (height unitwidth color1 color2))`

`height` specifies the height of the XPM

`unitwidth` specifies the unit that the width should scale to.  In most cases this is the width of a character as returned by `(frame-char-width)`.  It is important that the width be in the unit size of a character for the purposes of aligning text right.  While the `display` property of text can specify `space` and use `:align-to` to align to a particular pixel, there is no easy way to calculate the width that must be reserved for the right side.  The original implementation simple let the separators be whatever width they chose, but it meant that sometimes there was extra spacing on the right hand side because the fill width could not be accurately calculated.  The original implmentation also didn't calculate the reserve size.  While the fill width can be specified as a fraction of the character width, there is no simple way to calculate the pixel width of the right side.  That is, one would have to iterate through every item and calculate the total pixel width and so for now it's just easier to make dividers respect the width of characters.

`color1` the left-hand color

`color2` the right-hand color



