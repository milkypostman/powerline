powerline
=========

Emacs version of the Vim powerline.


I changed the name of this version to differentiate it from the original [Emacs Powerline](http://www.emacswiki.org/emacs/PowerLine) which is a fork of [vim-powerline](https://github.com/Lokaltog/vim-powerline).

This is a rewrite of the Emacs version because I thought the code was a little messy.  It also didn't allow the speparators (arrows) to be dynamically sized.  That means the mode-line was always required to be a particular size.  This version also allows the *right* side of the mode-line to be calculated ahead of time and the fill to be more accurate.  Thus, people can do more tweaking of the status bar.

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
                                  (powerline-raw "%12b" nil 'l)
    
                                  (powerline-arrow-right nil face1)
    
                                  (powerline-major-mode face1 'l)
                                  (powerline-minor-modes face1 'l)
                                  (powerline-raw mode-line-process face1 'l)
    
                                  (powerline-narrow face1 'l)
    
                                  (powerline-raw " " face1)
                                  (powerline-arrow-right face1 face2)
    
                                  (powerline-vc face2 'l)
                                  ))
                            (rhs (concat
                                  (powerline-raw global-mode-string face2 'r)
    
                                  (powerline-arrow-left face2 face1)
                                  (powerline-raw " " face1)
    
                                  (powerline-raw "%4l" face1 'r)
                                  (powerline-raw ":" face1)
                                  (powerline-raw "%3c" face1 'r)
    
                                  (powerline-arrow-left face1 nil)
                                  (powerline-raw " ")
    
                                  (powerline-raw "%6p" nil 'r)
    
                                  (powerline-hud face2 face1))))
                       (concat lhs (powerline-fill face2 (length (format-mode-line rhs))) rhs)))))

                       
The last line of this is what actually puts it all together.  But notice we pre-compile the `rhs` of the statusline and this allows us to more accurately right justify the text.  There are currently no other "separators", just the arrows.  I will be introducing more gradually.  This version should be easier to modify.


