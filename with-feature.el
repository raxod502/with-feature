;;; with-feature.el --- Better with-eval-after-load. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/with-feature
;; Keywords: extensions
;; Created: 6 Aug 2017

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; Libraries

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;;; Utility functions
;;;;; Functions

(defun with-feature-replace-symbol (form from to)
  "In FORM, recursively replace symbol FROM with symbol TO.
This function only recurses through cons cells, and not other
types of collections.

Warning: if you attempt to replace the symbol nil, one
side-effect will be to append to every list in the form, since
lists are nil-terminated implicitly."
  (cond
   ((consp form)
    (cons (with-feature-replace-symbol (car form) from to)
          (with-feature-replace-symbol (cdr form) from to)))
   ((eq form from) to)
   (t form)))

(defmacro with-feature-thread-anaphoric (it symbol &rest forms)
  "Anaphoric threading macro.
Binding IT to the given SYMBOL, evaluate the first of FORMS. Then
re-bind the result to SYMBOL, and evaluate the next of FORMS,
until FORMS are exhausted. Then return the result."
  (declare (indent 2))
  (if forms
      `(with-feature-thread-anaphoric
           (let ((,symbol ,it))
             ,(car forms))
           ,symbol
         ,@(cdr forms))
    it))

;;;;; Property lists

(defun with-feature-plist-remove (plist prop)
  "Remove a value from a property list.
PLIST is a property list and PROP is the key for the value to
remove. This is distinct to setting the value to nil using
`plist-put'. Keys are compared to PROP using `eq'.

The new plist is returned; the original is not modified."
  (cl-loop for (key val) on plist by #'cddr
           unless (eq key prop)
           collect key and collect val))

(defun with-feature-normalize-pseudo-plist (pseudo-plist)
  "Normalize a PSEUDO-PLIST into a regular plist.
The keywords of the pseudo-plist are taken to the keys of the
regular plist. The elements between keywords in the pseudo-plist
are collected into lists, which become the values of the regular
plist. If two keywords are adjacent, the first one has no effect.
Elements coming before any keywords have a key of nil in the
regular plist. If a keyword is specified more than once, the
latter overrides the former."
  (let ((keyword nil)
        (elts nil)
        (plist nil))
    (dolist (elt pseudo-plist (plist-put plist keyword elts))
      (if (keywordp elt)
          (progn
            (when elts
              (setq plist (plist-put plist keyword (nreverse elts))))
            (setq keyword elt)
            (setq elts nil))
        (push elt elts)))))

;;;;; Signals

(defun with-feature-error* (string &rest args)
  "Report an `error' from `with-feature'.
STRING and ARGS are as for `error'.

See also `with-feature-error' for the macro form of this
function."
  (apply #'error (format "with-feature: %s" string) args))

(defmacro with-feature-error (string &rest args)
  "Report an `error' from `with-feature'.
STRING and ARGS are as for `error'.

See also `with-feature-error*' for the function form of this
macro."
  `(error (format "with-feature: %s" ,string) ,@args))

(defun with-feature-only-one (list string &rest args)
  "Return the first element of LIST, or signal error.
An error is signalled if LIST has either more or fewer than one
item. STRING and ARGS are as for `error'."
  (unless (= (length list) 1)
    (apply #'with-feature-error* string args))
  (car list))

;;;;; Code generation

(defvar with-feature-recursive-autoload nil
  "Used to detect autoloading failure.
After an autoload stub has attempted to load the real definition,
it binds this variable before calling itself. That way, if the
real definition wasn't loaded, the stub will be called again and
it can tell what has happened.

Don't bind this yourself unless you like playing with
velociraptors.")

(defun with-feature-autoload
    (func feature &optional interactive prepare-func args)
  "Return code autoloading function FUNC from given FEATURE.
FUNC and FEATURE are symbols. INTERACTIVE tells whether the
function should be interactive. If PREPARE-FUNC is supplied, then
it is called with ARGS and it should return code that makes
FEATURE available in the current Emacs session. That code is not
evaluated until the autoload is triggered."
  (if prepare-func
      ;; We use a dynamically generated (per-function) symbol here
      ;; because one deferred autoload might trigger another after
      ;; being loaded, and that would not signal anything out of the
      ;; ordinary.
      (let ((recursive-autoload
             (intern (format "with-feature-recursive-autoload-%S"
                             func))))
        `(let* ((load-list-item '(defun . ,func))
                (already-loaded (member load-list-item current-load-list)))
           (defun ,func (&rest args)
             ,(format "Deferred autoload generated by `with-feature'.
This function is a stub created in order to autoload a function
provided by a feature that may not yet be available (%S).

\(fn ...)" feature)
             ,@(when interactive
                 `((interactive)))
             (cond
              ((bound-and-true-p ,recursive-autoload)
               (error "Loading feature `%S' did not define function `%S'"
                      ',feature ',func))
              (,(apply prepare-func args)
               (if (require ',feature nil 'noerror)
                   (let ((,recursive-autoload t))
                     (if (called-interactively-p 'any)
                         (call-interactively ',func)
                       (apply ',func args)))
                 (error "Deferred autoloading failed: feature `%S' unavailable"
                        ',feature)))
              (t (user-error "Feature loading unsuccessful, aborting"))))))
    `(autoload ,func ,(symbol-name feature) nil ,interactive)))

(defun with-feature-eval-or-defer
    (form &optional lazy-prepare-func &rest args)
  "Return code evalling FORM immediately or wrapping in runtime eval.
Deferral can only happen if LAZY-PREPARE-FUNC is provided. If it
is provided, it is called with ARGS, and a nil return value means
to defer evaluation until runtime."
  (if (and lazy-prepare-func (null (apply lazy-prepare-func args)))
      `(eval ',form)
    form))

(defun with-feature-maybe-progn (forms)
  "Wrap FORMS in a `progn' if necessary, else return the single form.
If no FORMS, return nil."
  (pcase (length forms)
    (0 nil)
    (1 (car forms))
    (_ `(progn ,@forms))))

;;;; Middleware handling

;;;###autoload
(defvar with-feature-middleware-alist nil
  "Alist of middleware to be applied by `with-feature'.
The keys are symbols; for symbol `foo' there should be a function
`with-feature-middleware/foo' which applies the middleware. The
values are numbers (not necessarily integers) that specify the
relative ordering of the middleware. Middleware with lower
ordering is applied first; in the case of a tie, the middleware
appearing first in the alist is applied first.

When `with-feature' receives its arguments, it passes them as a
list to the first middleware function, and threads the return
value through all remaining middleware functions. The eventual
expansion of the macro is the return value of the last middleware
function.

See also `with-feature-defmiddleware'.")

(defun with-feature-middlewares ()
  "Return a list of the middleware in `with-feature-keyword-alist', in order.
This means the middleware at the beginning of the list have the
lowest ordering, and are applied first by `with-feature'."
  (thread-first with-feature-middleware-alist
    (copy-sequence)
    (cl-stable-sort #'< :key #'cdr)
    (thread-last (mapcar #'car))))

(eval-and-compile
  (defun with-feature-middleware-handler (middleware)
    "Return the symbol for the function that handles given MIDDLEWARE symbol.
For example, if MIDDLEWARE is `split-args', then
`with-feature-middleware/split-args' is returned."
    (intern (format "with-feature-middleware/%S" middleware))))

;;;###autoload
(defmacro with-feature-defmiddleware
    (name order arglist &optional docstring &rest body)
  "Define and register a `with-feature' middleware.
NAME is an unquoted symbol, and ORDER is a number, possibly
computed at runtime. ARGLIST, DOCSTRING, and BODY are as in
`defun'.

Define a function of the form `with-feature-middleware/NAME', and
register the middleware in `with-feature-middleware-alist' with
the provided ORDER."
  (declare (indent defun))
  (unless (stringp docstring)
    (setq docstring nil)
    (setq body (cons docstring body)))
  `(progn
     (defun ,(with-feature-middleware-handler name) ,arglist
       ,@(when docstring
           (list docstring))
       ,@body)
     (map-put with-feature-middleware-alist ',name ,order)))

;;;; Core middleware
;;;;; Preprocessing middleware

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

;;;;; Keyword middleware

(with-feature-defmiddleware keyword-dialect 200 (state)
  "Validate `:dialect' from the `:plist', and copy it to the top level.
Remove from `:plist'."
  (let ((dialect-forms (thread-first state
                         (plist-get :plist)
                         (plist-get :dialect))))
    (when (> (length dialect-forms) 1)
      (with-feature-error
       "More than one form passed to :dialect: %S" dialect-forms))
    ;; If not specified, or keyword given without arguments, assume
    ;; nil dialect, the default.
    (let ((dialect (car dialect-forms)))
      (dolist (attribute dialect)
        (unless (symbolp attribute)
          (with-feature-error "Non-symbol `%S' passed to :dialect" attribute)))
      (with-feature-thread-anaphoric state it
        (plist-put it :dialect dialect)
        (plist-put it :plist (with-feature-plist-remove
                              (plist-get it :plist) :dialect))))))

(with-feature-defmiddleware keyword-init 300 (state)
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
    (plist-put it :plist (with-feature-plist-remove
                          (plist-get it :plist) :init))))

(with-feature-defmiddleware keyword-config 400 (state)
  "Copy the code under `:config' into `:deferred-code', and remove `:config'.
The code is inserted at the end of the accumulated
`:deferred-code' so that it can take advantage of automatically
generated autoloads, keybindings, and so on.

If `:config' is not specified, then no code is inserted."
  (with-feature-thread-anaphoric state it
    (plist-put it :deferred-code (append (plist-get it :deferred-code)
                                         (thread-first it
                                           (plist-get :plist)
                                           (plist-get :config))))
    (plist-put it :plist (with-feature-plist-remove
                          (plist-get it :plist) :config))))

(defvar with-feature-default-ensure-provider nil
  "Default value for `:ensure-provider' if none provided.")

(with-feature-defmiddleware keyword-ensure-provider 500 (state)
  "Copy `:ensure-provider' from `:plist' to top level.
If absent, substitute `with-feature-default-ensure-provider'. The
resulting value may be nil, so this must be checked later. Remove
`:ensure-provider' from `:plist'."
  (let ((provider (thread-first state
                    (plist-get :plist)
                    (plist-get :ensure-provider)
                    (or with-feature-default-ensure-provider))))
    (unless (symbolp provider)
      (with-feature-error
       "Non-symbol `%S' passed to :ensure-provider keyword" provider))
    (with-feature-thread-anaphoric state it
      (plist-put it :ensure-provider provider)
      (plist-put it :plist (thread-first it
                             (plist-get :plist)
                             (with-feature-plist-remove :ensure-provider))))))

(with-feature-defmiddleware keyword-ensure 600 (state)
  "Copy `:ensure' from `:plist' to top level.
Depending on dialect, maybe fill in a default value when not
provided. (Specifically, in a dialect with the `package'
attribute, the default is to use the feature name. Else the
default is nil, meaning `:ensure' has no effect.) Remove from
`:plist'."
  (let ((ensure (thread-first state
                  (plist-get :plist)
                  (plist-get :ensure)
                  (or (when (memq 'package (plist-get state :dialect))
                        (plist-get state :feature))))))
    (with-feature-thread-anaphoric state it
      (plist-put it :ensure ensure)
      (plist-put it :plist (thread-first it
                             (plist-get :plist)
                             (with-feature-plist-remove :ensure))))))

;;;;; Postprocessing middleware

(with-feature-defmiddleware check-unprocessed-keywords 700 (state)
  "Error if there are any unprocessed keywords left in `:plist'.
This includes a nil keyword (meaning some forms were placed in
`with-feature' before the first keyword). Remove `:plist'."
  (when-let ((plist (plist-get state :plist)))
    (if (car plist)
        (with-feature-error
         "Unknown keyword `%S'" (car plist))
      (with-feature-error
       "Code placed before first keyword: %S" (cadr plist))))
  (with-feature-plist-remove state :plist))

;; FIXME: here we must check the value of `:ensure' and
;; `:ensure-provider', and call out to the relevant ensure provider,
;; as well as possibly insert a runtime eval in the `wrap-deferred'
;; middleware.

(with-feature-defmiddleware wrap-deferred 800 (state)
  "Wrap `:deferred-code' in `with-eval-after-load', and add to `:code'.
Remove `:deferred-code'. The `with-eval-after-load' is placed at
the beginning of the code, so that the rest of the code can take
advantage of it. This assumes that the feature is registered
under `:feature'.

If no `:deferred-code' exists, don't add anything."
  (if-let ((deferred (plist-get state :deferred-code)))
      (with-feature-thread-anaphoric state it
        (plist-put it :code (cons `(with-eval-after-load
                                       ',(plist-get it :feature)
                                     ,@deferred)
                                  (plist-get it :code)))
        (with-feature-plist-remove it :deferred-code))
    state))

(with-feature-defmiddleware codegen 900 (state)
  "Generate the final code from key `:code' of STATE.
Assume that `:code' has a list of forms, and optionally wrap them
in a `progn' using `with-feature-maybe-progn' before returning
the final form."
  (with-feature-maybe-progn (plist-get state :code)))

;;;; Primary macro

(defvar with-feature-debug nil
  "Non-nil means print diagnostic info while expanding `with-feature' forms.")

;;;###autoload
(defmacro with-feature (feature &rest args)
  "After FEATURE is loaded, perform some actions based on ARGS."
  (declare (indent 1))
  (when with-feature-debug
    (message "Expanding (with-feature %S) with arguments %S."
             feature args))
  (let ((state (cons feature args)))
    (dolist (middleware (with-feature-middlewares)
                        (prog1 state
                          (when with-feature-debug
                            (message "Finished with-feature expansion."))))
      (let ((handler (with-feature-middleware-handler middleware)))
        (setq state (funcall handler state))
        (message "Middleware %S results in new state %S." middleware state)))))

;;;; Dialects

(defmacro with-feature-defdialect (name &rest dialect)
  "Declare alternate `with-feature' dialect as a user-facing macro.
NAME is an unquoted symbol naming the macro to be created.
DIALECT should contain unquoted symbols naming the default
dialect attributes assigned by this macro, which otherwise acts
identically to `with-feature'."
  (declare (indent defun))
  `(defmacro ,name (feature &rest args)
     ,(format "After FEATURE is loaded, perform some actions based on ARGS.
This function acts like `with-feature', but using the
%S dialect by default."
              dialect)
     (declare (indent 1))
     (when with-feature-debug
       (message "Expanding (%S %S) with arguments %S."
                ',name feature args)
       (message "Dialect is %S." ',dialect))
     (append (list 'with-feature feature :dialect ',dialect) args)))

(with-feature-defdialect with-package package)

(with-feature-defdialect with-deferred package deferred)

;;;; Closing remarks

(provide 'with-feature)

;;; with-feature.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
