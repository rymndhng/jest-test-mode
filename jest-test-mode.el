;;; jest-test-mode.el --- Minor mode for running Node.js tests using jest -*- lexical-binding: t; -*-

;; Author: Raymond Huang <rymndhng@gmail.com>
;; URL: https://github.com/rymndhng/jest-test-mode.el
;; Version: 0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This mode provides commands for running node tests using jest. The output is
;; shown in a separate buffer '*compilation*' in compilation mode. Backtraces
;; from failures and errors are marked and can be clicked to bring up the
;; relevent source file, where point is moved to the named line.
;;
;; The tests should be written with jest. File names are supposed to end in `.test.ts'
;;
;; Using the command `jest-test-run-at-point`, you can run test cases from the
;; current file.

;; Keybindings:
;;
;; C-c C-t n    - Runs the current buffer's file as an unit test or an rspec example.
;; C-c C-t p    - Runs all tests in the project
;; C-C C-t t    - Runs describe block at point
;; C-C C-t a    - Re-runs the last test command

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup jest-test nil
  "Minor mode providing commands for running jest tests in Node.js"
  :group 'js)

(defcustom jest-test-options
  '("--color")
  "Pass extra comand line options to jest when running tests."
  :initialize 'custom-initialize-default
  :type '(list)
  :group 'jest-test-mode)

(defcustom jest-test-npx-options
  '()
  "Pass extra command line arguments to npx when running tests."
  :initialize 'custom-initialize-default
  :type '(list)
  :group 'jest-test-mode)

(defvar jest-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t p")   'jest-test-run-all-tests)
    (define-key map (kbd "C-c C-t n")   'jest-test-run)
    (define-key map (kbd "C-c C-t a")   'jest-test-rerun-test)
    (define-key map (kbd "C-c C-t t")   'jest-test-run-at-point)
    (define-key map (kbd "C-c C-t d n") 'jest-test-debug)
    (define-key map (kbd "C-c C-t d a") 'jest-test-debug-rerun-test)
    (define-key map (kbd "C-c C-t d t") 'jest-test-debug-run-at-point)

    ;; (define-key map (kbd "C-c C-s")     'jest-test-toggle-implementation-and-test)
    map)
  "The keymap used in command `jest-test-mode' buffers.")

(define-minor-mode jest-test-mode
  "Toggle jest minor mode.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode"
  :init-value nil
  :lighter " Jest"
  :keymap 'jest-test-mode-map
  :group 'jest-test-mode)

(defmacro jest-test-from-project-directory (filename form)
  "Set to npm project root inferred from FILENAME and run the provided FORM with `default-directory` bound."
  (declare (indent 1))
  `(let ((default-directory (or (jest-test-project-root ,filename)
                                default-directory)))
     ,form))

(defmacro jest-test-with-debug-flags (form)
  "Execute FORM with debugger flags set."
  (declare (indent 0))
  `(let ((jest-test-options (seq-concatenate 'list jest-test-options (list "--runInBand") ))
         (jest-test-npx-options (seq-concatenate 'list jest-test-npx-options (list "--node-arg" "inspect"))))
     ,form))

(defun jest-test-project-root (filename)
  "Find project folder containing a package.json containing FILENAME."
  (if (jest-test-npm-project-root-p filename)
      filename
    (and filename
         (not (string= "/" filename))
         (jest-test-project-root
          (file-name-directory
           (directory-file-name (file-name-directory filename)))))))

(defun jest-test-npm-project-root-p (directory)
  "Check if DIRECTORY contain a package.json file."
  (file-exists-p (concat (file-name-as-directory directory) "/package.json")))

(defun jest-test-find-file ()
  "Find the testfile to run. Assumed to be the current file."
  (buffer-file-name))

(defvar jest-test-not-found-message "No test among visible bufers")

;;;###autoload
(defun jest-test-run ()
  "Run the current buffer's file as a test."
  (interactive)
  (let ((filename (jest-test-find-file)))
    (if filename
        (jest-test-from-project-directory filename
          (jest-test-run-command (jest-test-command filename)))
      (message jest-test-not-found-message))))

(defun jest-test-run-all-tests ()
  "Run all test in the project."
  (interactive)
  (jest-test-from-project-directory (buffer-file-name)
    (jest-test-run-command (jest-test-command ""))))

(defun jest-test-rerun-test ()
  "Run the previously run test in the project."
  (interactive)
  (jest-test-from-project-directory (buffer-file-name)
    (jest-test-run-command jest-test-last-test-command)))

(defun jest-test-run-at-point ()
  "Run the top level describe block of the current buffer's point."
  (interactive)
  (let ((filename (jest-test-find-file))
        (example  (jest-test-example-at-point)))
    (if (and filename example)
        (jest-test-from-project-directory filename
          (let ((jest-test-options (seq-concatenate 'list jest-test-options (list "-t" example))))
            (jest-test-run-command (jest-test-command filename))))
      (message jest-test-not-found-message))))

(defun jest-test-debug ()
  "Run the test with an inline debugger attached."
  (interactive)
  (jest-test-with-debug-flags
    (jest-test-run)))

(defun jest-test-debug-rerun-test ()
  "Run the test with an inline debugger attached."
  (interactive)
  (jest-test-with-debug-flags
    (jest-test-rerun-test)))

(defun jest-test-debug-run-at-point ()
  "Run the test with an inline debugger attached."
  (interactive)
  (jest-test-with-debug-flags
    (jest-test-run-at-point)))

(defun jest-test-example-at-point ()
  "Find the topmost describe block from where the cursor is and extract the name."
  (save-excursion
    (re-search-backward "^describe")
    (let ((text (thing-at-point 'line t)))
      (string-match "describe\(\\(.*\\)," text)
      (when-let ((example (match-string 1 text)))
        (substring example 1 -1)))))

(defvar jest-test-last-test-command
  nil
  "The last test command ran with.")

(defun jest-test-update-last-test (command)
  "Update the last test COMMAND."
  (setq jest-test-last-test-command command))

(defun jest-test-run-command (command)
  "Run compilation COMMAND in NPM project root."
  (jest-test-update-last-test command)
  (let ((comint-scroll-to-bottom-on-input t)
        (comint-scroll-to-bottom-on-output t)
        (comint-process-echoes t))
    ;; TODO: figure out how to prevent <RET> from re-sending the old input
    ;; See https://stackoverflow.com/questions/51275228/avoid-accidental-execution-in-comint-mode
    (compilation-start command t)))

;;;###autoload
(defun jest-test-command (filename)
  "Format test arguments for FILENAME."
  (format "npx %s jest %s %s"
          (mapconcat 'shell-quote-argument jest-test-npx-options " ")
          (mapconcat 'shell-quote-argument jest-test-options " ")
          filename))

;;; compilation-mode support

;; Source: https://emacs.stackexchange.com/questions/27213/how-can-i-add-a-compilation-error-regex-for-node-js
;; Handle errors that match this:
;; at addSpecsToSuite (node_modules/jest-jasmine2/build/jasmine/Env.js:522:17)
(add-to-list 'compilation-error-regexp-alist 'jest)
(add-to-list 'compilation-error-regexp-alist-alist
             '(jest "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3))

(defun jest-test-enable ()
  "Enable the jest test mode."
  (jest-test-mode 1))

(add-hook 'typescript-mode-hook 'jest-test-enable)
(add-hook 'js-mode-hook 'jest-test-enable)

(provide 'jest-test-mode)
;;; jest-test-mode.el ends here
