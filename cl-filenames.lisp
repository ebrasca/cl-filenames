(in-package :cl-filenames)

(defvar *DEFAULT-PATHNAME-DEFAULTS* nil)
;; ;; This should really have a host associated with it...
;; (defvar *default-pathname-defaults* (make-instance 'pathname :host nil))

;;user-homedir-pathname

(defgeneric file-stream-pathname (stream))

;;; host
;; TODO find-host (setf find-host)

(defgeneric host-name (host))

(defgeneric host-pathname-class (host)
  (:method (host) (find-class 'pathname)))

(defgeneric host-default-device (host))

;;; File System Host - base class that all file system hosts must superclass
;;;     Created so that find-host can identify file system host objects

(defclass file-system-host ()
  ())

;;; Logical pathnames.

(defclass logical-host (file-system-host)
  ((%name :initarg :name :reader host-name)
   (%translations :initform '() :accessor logical-host-translations)))

(defmethod host-pathname-class ((host logical-host))
  (find-class 'logical-pathname))

(defmethod host-default-device ((host logical-host))
  nil)

;;; utility

(defvar *valid-hostname-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-")

(defun valid-hostname-character-p (character)
  (find character *valid-hostname-characters*))

(defun mixed-case-p (name)
  (not (every (lambda (ch)
                (or (eql ch #\*)
                    (eql ch #\-)
                    (digit-char-p ch)
                    (upper-case-p ch)))
              name)))

(defun case-correct-path-component (component from-host to-host)
  (cond ((and (typep from-host 'logical-host)
              (not (typep to-host 'logical-host))
              (stringp component)
              (not (mixed-case-p component)))
         (string-downcase component))
        ((and (not (typep from-host 'logical-host))
              (typep to-host 'logical-host)
              (stringp component)
              (not (mixed-case-p component)))
         (string-upcase component))
        (t component)))

(defun translate-one (source from to what)
  (cond ((member (funcall what to) '(nil :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        ((and (equal (funcall what source) (funcall what from))
              (not (member (funcall what to) '(nil :wild :unspecified))))
         (funcall what to))
        ((or (equal (funcall what source) (funcall what from))
             (eql (funcall what from) :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        (t
         (error "Source and from ~S don't match." what))))

(defun translate-directory (source from-wildcard to-wildcard)
  (let* ((s-d (pathname-directory source))
         (f-d (pathname-directory from-wildcard))
         (t-d (pathname-directory to-wildcard))
         (new-path (list (first t-d))))
    (when (null f-d)
      (return-from translate-directory source))
    (loop ;; Match leading parts of source/from.
          (cond ((eql (first f-d) :wild)
                 (error ":WILD elements in from-wildcard directory not yet supported..."))
                ((eql (first f-d) :wild-inferiors)
                 (assert (null (rest f-d)) (source from-wildcard to-wildcard)
                         ":WILD-INFERIORS must be the last directory entry... (FIXME)")
                 (return))
                ((and (null s-d) (null f-d))
                 (return))
                ((or (null s-d)
                     (null f-d)
                     (not (equal (first s-d) (first f-d))))
                 (error "Directory entry mismatch. ~S ~S ~S ~S ~S~%"
                        (first s-d) (first f-d)
                        source from-wildcard to-wildcard)))
          (setf s-d (rest s-d)
                f-d (rest f-d)))
    ;; Merge SOURCE and TO. First component was done above.
    (do ((d (rest t-d) (cdr d)))
        ((or (null d)
             (eql (first d) :wild-inferiors))
         (cond ((null d)
                (assert (endp s-d) (s-d)
                        "To-wildcard directory portion exhausted with remaining source values.")
                (nreverse new-path))
               (t
                (assert (null (rest d))
                        (source from-wildcard to-wildcard)
                        ":WILD-INFERIORS must be the last directory entry... (FIXME)")
                (nconc (nreverse new-path)
                       (loop :for component :in s-d
                             :collect (case-correct-path-component component (pathname-host source) (pathname-host to-wildcard)))))))
      (push (first d) new-path))))

(defun pathname-match-directory (p w)
  (let ((p-dir (pathname-directory p))
        (w-dir (pathname-directory w)))
    (labels ((match (p w)
               (cond
                 ;; :wild-inferiors matches the remaining directory levels
                 ((eql (first w) :wild-inferiors) t)
                 ((and (null p) (null w)) t)
                 ((or (null p) (null w)) nil)
                 ((eql (first w) :wild)
                  (match (rest p) (rest w)))
                 (t (and (string= (first p) (first w))
                         (match (rest p) (rest w)))))))
      (and (eql (first p-dir) (first w-dir))
           (match (rest p-dir) (rest w-dir))))))

(defun extract-host-from-namestring (namestring)
  "Returns two values, the hostname extracted from NAMESTRING and the rest of the
path. If there is no host name, then NIL is returned as the first value and
NAMESTRING as the second."
  (do ((current 0 (1+ current))
       (hostname (make-array 50
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0)))
      ((>= current (length namestring))
       (values nil namestring))
    (let ((ch (char-upcase (char namestring current))))
      (when (eql ch #\:)
        ;; Reached end.
        (return (values hostname (subseq namestring (1+ current)))))
      (unless (valid-hostname-character-p ch)
        ;; Not a hostname.
        (return (values nil namestring)))
      (vector-push-extend ch hostname))))

;;; namestring

(defgeneric namestring-using-host (path host))

(defgeneric parse-namestring-using-host (host namestring junk-allowed))

(defun namestring (pathname)
  (let ((p (pathname pathname)))
    (concatenate 'string
                 (string (host-name (pathname-host p)))
                 ":"
                 (namestring-using-host (pathname-host p) p))))

(defun file-namestring (pathname)
  (namestring-using-host (pathname-host pathname)
                         (make-pathname :host (pathname-host pathname)
                                        :device nil
                                        :directory nil
                                        :name (pathname-name pathname)
                                        :type (pathname-type pathname)
                                        :version (pathname-version pathname)
                                        :defaults pathname)))

(defun directory-namestring (pathname)
  (namestring-using-host (pathname-host pathname)
                         (make-pathname :host (pathname-host pathname)
                                        :device nil
                                        :directory (pathname-directory pathname)
                                        :name nil
                                        :type nil
                                        :version nil
                                        :defaults pathname)))

(defun host-namestring (pathname)
  (host-name (pathname-host pathname)))

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  (if (eql (pathname-host pathname) (pathname-host defaults))
      (let ((p-dirs (pathname-directory pathname))
            (d-dirs (pathname-directory defaults)))
        (if (and (eq (car p-dirs) :absolute)
                 (eq (car d-dirs) :absolute))
            (do ((p-dir (cadr p-dirs) (cadr p-rest))
                 (d-dir (cadr d-dirs) (cadr d-rest))
                 (p-rest (cdr p-dirs) (cdr p-rest))
                 (d-rest (cdr d-dirs) (cdr d-rest)))
                ((or (null p-dir) (null d-dir) (not (equal p-dir d-dir)))
                 (cond ((null p-dir)
                        (if (null d-dir)
                            ;; directories match exactly
                            (file-namestring pathname)
                            ;; default directory has more entries than pathname
                            (namestring pathname)))
                       ((null d-dir)
                        (namestring-using-host (pathname-host pathname)
                                               (make-pathname
                                                :host (pathname-host pathname)
                                                :directory (cons :relative p-rest)
                                                :name (pathname-name pathname)
                                                :type (pathname-type pathname)
                                                :version (pathname-version pathname))))
                       ;; directory names differ
                       (t (namestring pathname)))))
            ;; pathnames are not absolute but have the same host
            (namestring-using-host (pathname-host pathname) pathname)))
      ;; hosts don't match
      (namestring pathname)))

(defun parse-namestring (thing &optional host (default-pathname *default-pathname-defaults*) &key (start 0) (end nil) junk-allowed)
  (loop :always (typep thing 'synonym-stream)
        :do (setf thing (symbol-value (synonym-stream-symbol thing))))
  (when (typep thing 'file-stream)
    (setf thing (file-stream-pathname thing)))
  (etypecase thing
    (pathname
     (unless (or (eql host nil)
                 (eql (pathname-host thing) host))
       (error 'simple-type-error
              :expected-type `(eql ,host)
              :datum (pathname-host thing)
              :format-control "Manifest host in pathname ~S does not match supplied host."
              :format-arguments (list thing)))
     thing)
    (string
     (setf thing (subseq thing start end))
     (multiple-value-bind (other-hostname rest-of-path)
         (extract-host-from-namestring thing)
       (when other-hostname
         ;; Namestring has an explicit host portion, do something with it.
         (let ((other-host (find-host other-hostname)))
           (cond ((and host (not (eql host other-host)))
                  (error 'simple-type-error
                         :expected-type `(eql ,host)
                         :datum other-host
                         :format-control "Manifest host in pathname ~S does not match supplied host."
                         :format-arguments (list thing)))
                 (t (setf host other-host)))))
       (setf default-pathname (pathname default-pathname))
       (parse-namestring-using-host (or host (pathname-host default-pathname))
                                    rest-of-path junk-allowed)))))

;;; pathname

(defclass pathname ()
  ((%host :initarg :host :reader host)
   (%device :initarg :device :reader device)
   (%directory :initarg :directory :reader directory)
   (%name :initarg :name :reader name)
   (%type :initarg :type :reader type)
   (%version :initarg :version :reader version))
  (:default-initargs :device nil :directory nil :name nil :type nil :version nil))

(defun pathname-host (pathname &key (case :local))
  (declare (ignore case))
  (host (pathname pathname)))

(defun pathname-device (pathname &key (case :local))
  (declare (ignore case))
  (device (pathname pathname)))

(defun pathname-directory (pathname &key (case :local))
  (declare (ignore case))
  (directory (pathname pathname)))

(defun pathname-name (pathname &key (case :local))
  (declare (ignore case))
  (name (pathname pathname)))

(defun pathname-type (pathname &key (case :local))
  (declare (ignore case))
  (type (pathname pathname)))

(defun pathname-version (pathname &key (case :local))
  (declare (ignore case))
  (version (pathname pathname)))

(defun pathnamep (object)
  (typep object 'pathname))

(defun pathname (pathname)
  (typecase pathname
    (pathname pathname)
    (file-stream
     (pathname (file-stream-pathname pathname)))
    ;; (synonym-stream
    ;;  (pathname (mezzano.internals::follow-synonym-stream pathname)))
    (string
     (parse-namestring pathname))
    (t
     (error 'type-error :datum pathname :expected-type 'pathname-designator))))

(defun make-pathname (&key host
                           (device nil devicep)
                           (directory nil directoryp)
                           (name nil namep)
                           (type nil typep)
                           (version nil versionp)
                           defaults)
  (let* ((defaults (cond (defaults
                          (pathname defaults))
                         ((pathname-host *default-pathname-defaults*)
                          (make-instance (host-pathname-class (pathname-host *default-pathname-defaults*))
                                         :host (pathname-host *default-pathname-defaults*)))
                         (t
                          ;; During bootstrap, before any real filesystems are
                          ;; defined, the pathname in *D-P-D* is a hostless
                          ;; pathname. We want to avoid creating any more of
                          ;; these. So just use it as-is. This is fine, as all
                          ;; the other elements are NIL anyway.
                          *default-pathname-defaults*)))
         (host (if host
                   (find-host host)
                   (pathname-host defaults))))
    (make-instance (host-pathname-class host)
                   :host host
                   :device (if devicep device (pathname-device defaults))
                   :directory (if directoryp
                                  (if (eq directory :wild)
                                      '(:absolute :wild-inferiors)
                                      directory)
                                  (pathname-directory defaults))
                   :name (if namep name (pathname-name defaults))
                   :type (if typep type (pathname-type defaults))
                   :version (if versionp version (pathname-version defaults)))))

;; (defmethod initialize-instance :after ((instance pathname) &key)
;;   (assert (host instance)))

;;; logical-pathname

(defclass logical-pathname (pathname)
  ())

;; TODO
;; Function LOAD-LOGICAL-PATHNAME-TRANSLATIONS

(defun logical-pathname-translations (host)
  (let ((host (or (find-host host nil)
                  (error "Logical host ~S not yet defined." host))))
    (check-type host logical-host)
    (logical-host-translations host)))

(defun (setf logical-pathname-translations) (new-translations host)
  (let ((logical-host (find-host host nil)))
    (unless logical-host
      (check-type host string)
      (setf logical-host (make-instance 'logical-host :name (string-upcase host))
            (find-host host) logical-host))
    (check-type logical-host logical-host)
    (setf (logical-host-translations logical-host) new-translations)))

(defun logical-pathname (pathspec)
  (let ((pathname (pathname pathspec)))
    (check-type pathname logical-pathname)
    pathname))

(defmethod namestring-using-host ((host logical-host) (path logical-pathname))
  (with-output-to-string (namestring)
    (when (eql (first (pathname-directory path)) :relative)
      (write-char #\; namestring))
    (flet ((write-word (word)
             (cond ((mixed-case-p word)
                    (write-char #\| namestring)
                    (loop :for ch :across word
                          :do (when (member ch '(#\\ #\|))
                                (write-char #\\ namestring))
                              (write-char ch namestring))
                    (write-char #\| namestring))
                   (t
                    (write-string word namestring)))))
      (dolist (dir (rest (pathname-directory path)))
        (cond ((eql dir :wild)
               (write-string "*" namestring))
              ((eql dir :wild-inferiors)
               (write-string "**" namestring))
              (t
               (write-word dir)))
        (write-char #\; namestring))
      (let ((name (pathname-name path))
            (type (pathname-type path))
            (version (pathname-version path)))
        (cond ((eql name :wild)
               (write-string "*" namestring))
              (name
               (write-word name)))
        (when type
          (write-char #\. namestring)
          (cond ((eql type :wild)
                 (write-string "*" namestring))
                (t
                 (write-word type)))
          (when version
            (write-char #\. namestring)
            (cond ((eql version :wild)
                   (write-string "*" namestring))
                  ((eql version :newest)
                   (write-string "NEWEST" namestring))
                  (t
                   (write type namestring)))))))))

(defmethod parse-namestring-using-host ((host logical-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (let ((relative :absolute)
        (directories '())
        (name nil)
        (type nil)
        (version nil)
        (offset 0))
    (flet ((consume-char (char)
             (when (and (< offset (length namestring))
                        (char= (char namestring offset) char))
               (incf offset)
               t))
           (consume-word ()
             (let ((chars (make-array 50
                                      :element-type 'character
                                      :fill-pointer 0
                                      :adjustable t))
                   (saw-escape nil))
               (loop
                 (when (>= offset (length namestring))
                   (when saw-escape
                     (error "Unexpected end of string after escape in logical pathname"))
                   (return))
                 (let ((ch (char namestring offset)))
                   (cond ((and (eql saw-escape :multiple)
                               (eql ch #\|))
                          ;; Leave multiple escape.
                          (setf saw-escape nil)
                          (incf offset))
                         ((and (eql saw-escape :multiple)
                               (eql ch #\\))
                          (setf saw-escape :multiple-single)
                          (incf offset))
                         (saw-escape
                          (vector-push-extend ch chars)
                          (case saw-escape
                            (:single
                             (setf saw-escape nil))
                            (:multiple-single
                             (setf saw-escape :multiple)))
                          (incf offset))
                         ((or (alphanumericp ch)
                              (eql ch #\-)
                              (eql ch #\*))
                          (vector-push-extend (char-upcase ch) chars)
                          (incf offset))
                         ((eql ch #\\)
                          (setf saw-escape :single)
                          (incf offset))
                         ((eql ch #\|)
                          (setf saw-escape :multiple)
                          (incf offset))
                         (t
                          (return)))))
               (cond ((string= chars "*")
                      :wild)
                     ((string= chars "**")
                      :wild-inferiors)
                     ((string= chars "")
                      nil)
                     (t chars)))))
      ;; [relative-directory-marker]
      (when (consume-char #\;)
        (setf relative :relative))
      ;; {directory directory-marker}* [name]
      (loop :for word := (consume-word)
            :always word
            :do (cond ((not (consume-char #\;))
                       ;; This is the name portion.
                       (when (eql word :wild-inferiors)
                         (error "Unexpected wild-inferiors in name position."))
                       (setf name word)
                       (return))
                      (t
                       ;; Directory element.
                       (push word directories))))
      ;; Possible type & version.
      (when (consume-char #\.)
        (let ((word (or (consume-word)
                        (error "Expected type after type-marker."))))
          (when (eql word :wild-inferiors)
            (error "Unexpected wild-inferiors in type position."))
          (setf type word)
          (when (consume-char #\.)
            (let ((word (or (consume-word)
                            (error "Expected version after version-marker."))))
              (when (eql word :wild-inferiors)
                (error "Unexpected wild-inferiors in version position."))
              (cond ((and (stringp word)
                          (every #'digit-char-p word))
                     (setf version (parse-integer word)))
                    ((and (stringp word)
                          (string= word "NEWEST"))
                     (setf version :newest))
                    (t (setf version word))))))))
    (unless (eql offset (length namestring))
      (error "Unexpected ~C in logical namestring." (char namestring offset)))
    (make-instance 'logical-pathname
                   :host host
                   :device :unspecific
                   :directory (list* relative (reverse directories))
                   :name name
                   :type type
                   :version version)))

;;; <<< ???

(defun wild-pathname-p (pathname &optional field-key)
  (ecase field-key
    ((nil) (or (eql (pathname-host pathname) :wild)
               (eql (pathname-device pathname) :wild)
               (eql (pathname-directory pathname) :wild)
               (and (listp (pathname-directory pathname))
                    (or (find :wild (cdr (pathname-directory pathname)))
                        (find :wild-inferiors (cdr (pathname-directory pathname)))))
               (eql (pathname-name pathname) :wild)
               (eql (pathname-type pathname) :wild)
               (eql (pathname-version pathname) :wild)))
    (:host (eql (pathname-host pathname) :wild))
    (:device (eql (pathname-device pathname) :wild))
    (:directory (or (eql (pathname-directory pathname) :wild)
                    (and (listp (pathname-directory pathname))
                         (or (find :wild (cdr (pathname-directory pathname)))
                             (find :wild-inferiors (cdr (pathname-directory pathname)))))))
    (:name (eql (pathname-name pathname) :wild))
    (:type (eql (pathname-type pathname) :wild))
    (:version (eql (pathname-version pathname) :wild))))

(defun pathname-match-p (pathname wildcard)
  (let ((p (pathname pathname))
        (w (pathname wildcard)))
    (and (or (member (pathname-host w) '(nil :wild))
             (eql (pathname-host p) (pathname-host w)))
         (or (member (pathname-device w) '(nil :wild))
             (equal (pathname-device p) (pathname-device w)))
         (pathname-match-directory p w)
         (or (member (pathname-name w) '(nil :wild))
             (equal (pathname-name p) (pathname-name w)))
         (or (member (pathname-type w) '(nil :wild))
             (equal (pathname-type p) (pathname-type w)))
         (or (member (pathname-version w) '(nil :wild))
             (equal (pathname-version p) (pathname-version w))))))

(defun translate-logical-pathname (pathname &key)
  (let ((pathname (pathname pathname)))
    (unless (typep pathname 'logical-pathname)
      (return-from translate-logical-pathname pathname))
    (loop :with host := (pathname-host pathname)
          :for translation :in (logical-pathname-translations host)
          :for from-wildcard := (parse-namestring (first translation) host)
          :for to-wildcard := (pathname (second translation))
          :do (when (pathname-match-p pathname from-wildcard)
                (return (translate-logical-pathname
                         (translate-pathname pathname from-wildcard to-wildcard))))
          :finally
          (error "No matching translation for logical pathname ~S." pathname))))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (make-pathname :host (pathname-host to-wildcard)
                 :device (cond ((typep source 'logical-pathname)
                                ;; Always favour the to-wildcard's device when
                                ;; translating from a logical pathname.
                                ;; Logical pathnames always have a device of
                                ;; :UNSPECIFIC, which would otherwise override
                                ;; any device specified in a translation.
                                (pathname-device to-wildcard))
                               (t
                                (translate-one source from-wildcard to-wildcard 'pathname-device)))
                 :name (translate-one source from-wildcard to-wildcard 'pathname-name)
                 :type (translate-one source from-wildcard to-wildcard 'pathname-type)
                 :version (translate-one source from-wildcard to-wildcard 'pathname-version)
                 :directory (translate-directory source from-wildcard to-wildcard)))

(defun merge-pathnames (pathname &optional
                                   (default-pathname *default-pathname-defaults*)
                                   (default-version :newest))
  (let* ((default-pathname (pathname default-pathname))
         (pathname (let ((*default-pathname-defaults* default-pathname))
                     (pathname pathname)))
         (host (or (pathname-host pathname) (pathname-host default-pathname)))
         (device (pathname-device pathname))
         (directory (pathname-directory pathname))
         (name (or (pathname-name pathname) (pathname-name default-pathname)))
         (type (or (pathname-type pathname) (pathname-type default-pathname)))
         (version (or (pathname-version pathname)
                      (if (pathname-name pathname)
                          default-version
                          (pathname-version default-pathname)))))
    (when (and (not device)
               (pathname-host pathname)
               (not (pathname-device pathname)))
      (if (and (eql (pathname-host pathname) (pathname-host default-pathname)))
          (setf device (pathname-device default-pathname))
          (setf device (host-default-device host))))
    (cond ((and (pathname-directory default-pathname)
                (eql (first directory) :relative))
           (setf directory (append (pathname-directory default-pathname)
                                   (rest directory)))
           ;; remove :backs
           (let ((dir-type (car directory))
                 (dirs))
             (dolist (d (cdr directory))
               (if (eq d :back)
                   (pop dirs)
                   (push d dirs)))
             (setf directory (cons dir-type (nreverse dirs)))))
          ((null directory)
           (setf directory (pathname-directory default-pathname))))
    (make-pathname :host host
                   :device device
                   :directory directory
                   :name name
                   :type type
                   :version version)))
