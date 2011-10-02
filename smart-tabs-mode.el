;; Emacs smart tabs mode
;;   Intelligently indent with tabs, align with spaces!
;;
;; To use, save smart-tabs-mode.el to your .emacs.d directory, and add
;; the following to your .emacs file:
;;
;;  (require 'smart-tabs-mode)
;;
;; Then, for each language you want to use smart tabs, set up a hook
;; and advice like so:
;;
;;  (add-hook 'MODE-HOOK 'smart-tabs-mode-enable)
;;  (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)
;;
;; Here are some specific examples for a few popular languages:
;;
;;  ;; C/C++
;;  (add-hook 'c-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice c-indent-line c-basic-offset)
;;  (smart-tabs-advice c-indent-region c-basic-offset)
;;
;;  ;; JavaScript
;;  (add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice js2-indent-line js2-basic-offset)
;;
;;  ;; Perl (cperl-mode)
;;  (add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice cperl-indent-line cperl-indent-level)
;;
;;  ;; Python
;;  (add-hook 'python-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice python-indent-line-1 python-indent)
;;
;;  ;; Ruby
;;  (add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice ruby-indent-line ruby-indent-level)
;;
;;
;; smart-tabs-mode.el is licensed under the GPLv2.
;; It is derived from http://www.emacswiki.org/emacs/SmartTabs
;;
;;
;; Modifications by John Croisant:
;;  * Remembers and re-applies window start, so that indenting doesn't
;;    cause the window to visibly scroll. (2009-09-18)
;;  * Reverted the above. The original problem seems to be gone, and
;;    my change causes a bug when newline-and-indent is used on the
;;    bottom line visible in the window. (2011-09-21)
;;  * Turned smart tabs into a minor mode. (2011-09-30)
;;
;; Modifications by Julien Fontanet:
;;  * Apply to languages other than C/C++. (2011-03-31)
;;
;; Modifications by Tomita Hiroshi:
;;  * Set indent-tabs-mode automatically for files with tabs. (2011-06-03)


(defmacro smart-tabs-mode/no-tabs-mode-advice (function)
  `(unless (ad-find-advice ',function 'around 'smart-tabs)
     (defadvice ,function (around smart-tabs activate)
       (if smart-tabs-mode
           (let ((indent-tabs-mode nil)) ad-do-it)
         ad-do-it))))


(define-minor-mode smart-tabs-mode
  "Intelligently indent with tabs, align with spaces!"

  (progn
    (smart-tabs-mode/no-tabs-mode-advice align)
    (smart-tabs-mode/no-tabs-mode-advice align-regexp)
    (smart-tabs-mode/no-tabs-mode-advice indent-relative)

    (unless
        (ad-find-advice 'indent-according-to-mode 'around 'smart-tabs)
      (defadvice indent-according-to-mode (around smart-tabs activate)
        (if smart-tabs-mode
            (let ((indent-tabs-mode indent-tabs-mode))
              (if (memq indent-line-function
                        '(indent-relative
                          indent-relative-maybe))
                  (setq indent-tabs-mode nil))
              ad-do-it)
          ad-do-it)))
    ))

(defun smart-tabs-mode-enable ()
  "Enable smart-tabs-mode."
  (smart-tabs-mode t))


(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (smart-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((indent-tabs-mode t)
               (tab-width fill-column)
               (,offset fill-column))
           (unwind-protect
               (progn ad-do-it))))
        (t
         ad-do-it)))))


(provide 'smart-tabs-mode)
