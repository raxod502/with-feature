;;; with-feature-core.el --- Core middleware. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the core middleware for with-feature. It is
;; separate from with-feature.el because compiling it requires the use
;; of functions defined in with-feature.el, and putting this in the
;; same file would mean with-feature.el would need a lot of
;; `eval-and-compile' forms wrapping its function definitions.

;;; Code:

(require 'subr-x)
(require 'with-feature)

(with-feature-defmiddleware split-args 0 (args)
  "Extract feature and pseudo-plist from `with-feature' ARGS.
Return a plist of the feature name as a symbol under `:feature',
and the pseudo-plist under `:pseudo-plist'.

Raise an error if the feature name is not a symbol, but otherwise
do no validation.

Assume that the ARGS are a list of at least one element."
  (unless (symbolp (car args))
    (with-feature-error "Feature `%S' is not symbol" (car args)))
  `(:feature ,(car args) :pseudo-plist ,(cdr args)))

(with-feature-defmiddleware normalize-pseudo-plist 100 (state)
  "Normalize the pseudo-plist under key `:pseudo-plist' of STATE.
Remove its key and put the normalized plist under key `:plist'.
The normalization is done using
`with-feature-normalize-pseudo-plist'."
  (let* ((pseudo-plist (plist-get state :pseudo-plist))
         (plist (with-feature-normalize-pseudo-plist pseudo-plist)))
    (thread-first state
      (with-feature-plist-remove :pseudo-plist)
      (plist-put :plist plist))))

(with-feature-defmiddleware keyword-init 200 (state)
  "Copy the code under `:init' into `:code', and remove `:init' key.
The code is inserted at the end of the accumulated code so that
the user can take advantage of keybindings and autoloads that
were previously defined by other keywords.

If `:init' is not specified, then no code is inserted."
  (with-feature-thread-anaphoric state it
    (plist-put it :code (append (plist-get it :code)
                                (thread-first it
                                  (plist-get :plist)
                                  (plist-get :init))))
    (with-feature-plist-remove :init)))

(with-feature-defmiddleware codegen 200 (state)
  "Generate the final code from key `:code' of STATE.
Assume that `:code' has a list of forms, and optionally wrap them
in a `progn' using `with-feature-maybe-progn' before returning
the final form."
  (with-feature-maybe-progn (plist-get state :code)))

(provide 'with-feature-core)

;;; with-feature-core.el ends here
