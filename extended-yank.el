;;; extended-yank.el --- Utilities for extended yanking -*- lexical-binding: t; -*-
;; Version: 1.1

(defgroup extended-yank nil
  "Customization group for extended-yank."
  :group 'editing)

(defcustom extended-yank-always-convert-to-org nil
  "When non-nil, always convert HTML to Org-mode format regardless of major mode.
When nil (the default), conversion only happens in `org-mode`."
  :type 'boolean
  :group 'extended-yank)

(defcustom extended-yank-always-convert-to-markdown nil
  "When non-nil, always convert HTML to Markdown format regardless of major mode.
When nil (the default), conversion only happens in `markdown-mode`."
  :type 'boolean
  :group 'extended-yank)

(defcustom extended-yank-always-convert-to-latex nil
  "When non-nil, always convert HTML to LaTeX format regardless of major mode.
When nil (the default), conversion only happens in `latex-mode`."
  :type 'boolean
  :group 'extended-yank)

(defcustom extended-yank-filter-html t
  "When non-nil (the default), filter HTML using a Lua filter to clean it up.
This removes elements like images and strips attributes from links and headers."
  :type 'boolean
  :group 'extended-yank)



(defun extended-yank--fetch-macos-html ()
  "Fetch and decode HTML from macOS clipboard using osascript and ruby."
  (let* ((osascript-cmd 
          "osascript -e 'get the clipboard as «class HTML»' | sed 's/«data HTML//; s/»//' | xxd -r -p ")
         (html-output (shell-command-to-string osascript-cmd)))
    (if (and html-output (not (string-empty-p html-output)))
        html-output
      nil)))

(defun extended-yank--fetch-windows-html ()
  "Fetch HTML from Windows clipboard using PowerShell."
  (let* ((ps-cmd "powershell.exe -Command \"Get-Clipboard -TextFormatType Html\"")
         (html-output (shell-command-to-string ps-cmd)))
    ;; Windows Get-Clipboard -TextFormatType Html includes a CF_HTML header.
    ;; Pandoc can usually handle it, but if we need to be precise, we could strip it.
    ;; For now, let's return it as is and see if Pandoc handles it correctly.
    (if (and html-output (not (string-empty-p html-output)))
        html-output
      nil)))

(defun extended-yank--fetch-linux-html ()
  "Fetch HTML from Linux clipboard using xclip or wl-paste."
  (let ((cmd (cond
              ((executable-find "wl-paste") "wl-paste -t text/html")
              ((executable-find "xclip") "xclip -selection clipboard -t text/html -o")
              (t nil))))
    (if cmd
        (let ((html-output (shell-command-to-string cmd)))
          (if (and html-output (not (string-empty-p html-output)))
              html-output
            nil))
      (message "Extended-Yank: Neither 'xclip' nor 'wl-paste' found on Linux.")
      nil)))

(defun extended-yank--fetch-html-from-clipboard ()
  "Fetch HTML from clipboard based on the operating system."
  (cond
   ((eq system-type 'windows-nt) (extended-yank--fetch-windows-html))
   ((eq system-type 'darwin) (extended-yank--fetch-macos-html))
   ((eq system-type 'gnu/linux) (extended-yank--fetch-linux-html))))

(defun extended-yank--run-pandoc (html &optional to-format)
  "Pipe HTML through pandoc with a portable Lua filter.
TO-FORMAT defaults to \"org\"."
  (if (not (executable-find "pandoc"))
      (progn
        (warn "Extended-Yank: 'pandoc' not found in PATH.")
        "")
    (let* ((to-format (or to-format "org"))
           (lua-filter-content "
function Image(el) return {} end
function Link(el) el.attr = pandoc.Attr(); return el end
function Header(el) el.attr = pandoc.Attr(); return el end
")
           (lua-file (make-temp-file "pandoc-filter-" nil ".lua")))
      (unwind-protect
          (progn
            (with-temp-file lua-file
              (insert lua-filter-content))
            (with-temp-buffer
              (insert html)
              (let* ((coding-system-for-write 'utf-8)
                     (extra-args (if (string= to-format "markdown") "--wrap=none" ""))
                     (pandoc-cmd (format "pandoc -f html -t %s %s %s"
                                         to-format
                                         extra-args
                                         (if extended-yank-filter-html
                                             (format "--lua-filter=%s" (shell-quote-argument lua-file))
                                           ""))))
                (shell-command-on-region (point-min) (point-max) pandoc-cmd nil t))
              (buffer-string)))
        (when (file-exists-p lua-file)
          (delete-file lua-file))))))


(defun extended-yank-yank-html-as-org (&optional html)
  "Paste HTML from clipboard as Org-mode content.
If HTML is provided, use it instead of fetching from clipboard."
  (interactive)
  (let ((source-html (or html (extended-yank--fetch-html-from-clipboard))))
    (when (and source-html (not (string-empty-p source-html)))
      (let ((result (extended-yank--run-pandoc source-html "org")))
        (unless (string-empty-p result)
          (insert result))))))

(defun extended-yank-yank-html-as-markdown (&optional html)
  "Paste HTML from clipboard as Markdown content.
If HTML is provided, use it instead of fetching from clipboard.
Passes --wrap=none to pandoc for Markdown."
  (interactive)
  (let ((source-html (or html (extended-yank--fetch-html-from-clipboard))))
    (when (and source-html (not (string-empty-p source-html)))
      (let ((result (extended-yank--run-pandoc source-html "markdown")))
        (unless (string-empty-p result)
          (insert result))))))

(defun extended-yank-yank-html-as-latex (&optional html)
  "Paste HTML from clipboard as LaTeX content.
If HTML is provided, use it instead of fetching from clipboard."
  (interactive)
  (let ((source-html (or html (extended-yank--fetch-html-from-clipboard))))
    (when (and source-html (not (string-empty-p source-html)))
      (let ((result (extended-yank--run-pandoc source-html "latex")))
        (unless (string-empty-p result)
          (insert result))))))

(defun extended-yank-yank-html (html &optional _arg &rest _)
  "Convert HTML to Org, Markdown or LaTeX based on context.
If HTML is nil, the specific functions will fetch it."
  (interactive (list nil current-prefix-arg))
  (cond
   ((or extended-yank-always-convert-to-org
        (derived-mode-p 'org-mode))
    (extended-yank-yank-html-as-org html))
   
   ((or extended-yank-always-convert-to-markdown
        (derived-mode-p 'markdown-mode))
    (extended-yank-yank-html-as-markdown html))
   
   ((or extended-yank-always-convert-to-latex
        (derived-mode-p 'latex-mode))
    (extended-yank-yank-html-as-latex html))
   
   (t
    ;; Fallback: Insert raw HTML
    (let ((source-html (or html (extended-yank--fetch-html-from-clipboard))))
      (when (and source-html (not (string-empty-p source-html)))
        (insert source-html))))))


(provide 'extended-yank)
