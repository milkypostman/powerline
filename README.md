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

