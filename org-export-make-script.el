;;; package --- Summary

;;; Commentary:

;; Cases:
;; - construct path from plist property or filename + extension
;; - construct argument + path from existence of file in folder

;; Look for defaults.

;;; Code:

(defun get-arg-from-plists (infile property default)
  "Look in files under INFILE for PROPERTY from file plist el.
Look for the file under: 
./INFILE/plist.el and ./config/plist.el.
If no file or property is found, return nil.

Used for setting:
1. the output path,
2. the script path and
3. the rsync command.

Note: This function does not return paths of found files,
but only data either either found in plist.el or given by DEFAULT.
"
  (let (user-config)
    (-first
     (lambda (fname)
       (and (file-exists-p fname)
            (setq user-config
                  (plist-get
                   (cadar
                    (read-from-string
                     (with-temp-buffer
                       (insert-file-contents fname) (buffer-string))))
                   property))))
     ;; find plist.el in path obtained from infile, expanded to full path
     (let ((config-file "plist.el"))
       (mapcar 'file-truename
               (make-config-paths infile "plist.el"))))
    (or user-config (replace-regexp-in-string "%s" infile default))))

(defun make-config-paths (infile config-file)
  (setq infile (file-name-sans-extension (file-truename infile)))
  (list
   (format "%s/%s" infile config-file)
   (format "%sconfig/%s" (file-name-directory infile) config-file)))

(defun make-command-arg-from-file (infile datafile foundstring defaultstring)
  "Given:
1. the names of an input file
2. the name of a file that is to be used as part of a command
3. A command template string

Construct a command argument, depending on whether the datafile was
found and where.

Used for setting:
1. the css stylesheet argument,
2. --bootstrap or --incude directive,
3. the additional header argument. "
  (let ((path
         (-first 'file-exists-p
                 (make-config-paths infile datafile))))
    (if path
        (replace-regexp-in-string
         "%s"
         (concat
          "./"
          (file-name-nondirectory
           (directory-file-name (file-name-directory path)))
          "/"
          (file-name-nondirectory path))
         foundstring)
      defaultstring)))

(defun make-export-script (filename)
  (let* ((truename (file-truename filename))
         (basename (file-name-sans-extension truename))
         (name-only (file-name-nondirectory basename)))
    (concat
     "cd "
     (file-name-directory truename)
     "\norg-export --infile ./"
     (file-name-nondirectory filename)
     " --outfile "
     (get-arg-from-plists filename :outfile (format "./%s.html" name-only))
     (make-command-arg-from-file
      filename "style.css"
      " --css %s --embed-css" " --bootstrap --embed-css")
     "\ncd "
     (file-name-directory truename)
     "\n"
     (get-arg-from-plists
      filename :rsync
      (format "#rsync -avz ./%s /user@domain.org:/output/"
              (file-name-nondirectory filename))))))

(defun make-and-save-export-script (&optional filename)
  "Note: you need to run source ~/.zshrc or source ./bashrc
in order to make the newly linked shell command available in your shell."
  (interactive "FChoose a file to make its script: ")
  (find-file (concat (file-name-sans-extension filename) ".sh"))
  (insert (make-export-script filename))
  (save-buffer)
  (set-file-modes (buffer-file-name) #o777)
  (make-symbolic-link
   (buffer-file-name)
   (file-truename (concat
                   "~/.org-export/"
                   (file-name-nondirectory (buffer-file-name))))))

(defun open-export-edit-config-folders (&optional filename)
  (interactive "FChoose a file to make its script: ")
  (setq filename (file-truename (file-name-sans-extension filename)))
  (unless (file-exists-p filename) (make-directory filename))
  (dired filename)
  (split-window-vertically)
  (setq filename (concat (file-name-directory filename) "/config"))
  (unless (file-exists-p filename) (make-directory filename))
  (dired filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-export-make-script)
;;; org-export-make-script.el ends here
