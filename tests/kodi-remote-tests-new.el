

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  (package-initialize))

;; (unload-feature 'kodi-remote)
(load (concat (getenv "PWD") "/kodi-remote"))



(defun kodi-series-response-data ()
  "Kodi Json example response to VideoLibrary.GetTVShows request"
  '((limits (end . 2) (start . 0) (total . 2))
    (tvshows . [((episode . 1) (file . "/PATH/SERIES1")
		 (label . "Series 1") (title . "Series 1")
		 (tvshowid . 1) (watchedepisodes . 1))
		((episode . 7) (file . "/PATH/SERIES2")
		 (label . "Series 2") (title . "Series 2")
		 (tvshowid . 2) (watchedepisodes . 5))])))


(defun kodi-movies-response-data ()
  "Kodi Json example response to VideoLibrary.GetMovies request"
  '((limits (end . 2) (start . 0) (total . 2))
    (movies . [((label . "Movie 1") (movieid . 1)(file . "/PATH/Movie 1"))
	       ((label . "Movie 2") (movieid . 2)(file . "/PATH/Movie 2"))])))

(defun kodi-episodes-response-data ()
  "Kodi Json example response to VideoLibrary.GetEpisodes request"
  '((limits (end . 2) (start . 0) (total . 2))
    (episodes . [((episode . 1) (episodeid . 10)
		  (file . "/PATH/Foo/Foo.S01E01.mkv")
		  (label . "1x01. Title 1") (playcount . 0)
		  (title . "Title 1"))
		 ((episode . 2) (episodeid . 20)
		  (file . "/PATH/Foo/Foo.S01E02.mkv")
		  (label . "1x02. Title 2") (playcount . 0)
		  (title . "Title 2"))])))


(ert-deftest test-draw-series ()
  "Tests series Buffer generation"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       (kodi-remote-get (a b)
			:ordered nil
			((:input '("Files.GetSources"
				   (("params"
				     ("media" . "video"))))
				 :min-occur 0)
			 (:input '("VideoLibrary.GetTVShows"
				   (("params"
				     ("properties" .
				      [title watchedepisodes
					     episode file]))))
				 :min-occur 0))))
    (let* ((kodi-properties (kodi-series-response-data))
	   (kodi-watch-filter "all")
	   (kodi-show-df nil))
      (kodi-remote-draw-shows)
      (should (equal mode-name "kodi-remote-series: all"))
      (should (equal tabulated-list-format
		     '[("entries" 10 t)
		       ("Series" 30 t)]))
      (should (equal tabulated-list-entries
		     '((1 [("1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)
			   ("Series 1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)])
		       (2 [("7" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("Series 2" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)])))))))


(ert-deftest test-draw-series-seen ()
  "Tests series Buffer generation with seen filter"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       (kodi-remote-get (a b)
			:ordered nil
			((:input '("Files.GetSources"
				   (("params"
				     ("media" . "video"))))
				 :min-occur 0)
			 (:input '("VideoLibrary.GetTVShows"
			 	   (("params"
			 	     ("properties" .
			 	      [title watchedepisodes
			 		     episode file])
			 	     ("filter" ("field" . "playcount")
				      ("operator" . "greaterthan")
				      ("value" . "0")))))
				 :min-occur 0))))
    (let* ((kodi-properties (kodi-series-response-data))
	   (kodi-watch-filter "seen")
	   (kodi-show-df nil))
      (kodi-remote-draw-shows)
      (should (equal mode-name "kodi-remote-series: seen"))
      (should (equal tabulated-list-format
		     '[("entries" 10 t)
		       ("Series" 30 t)]))
      (should (equal tabulated-list-entries
		     '((1 [("1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)
			   ("Series 1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)])
		       (2 [("5" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("Series 2" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)])))))))

(ert-deftest test-draw-series-unseen ()
  "Tests series Buffer generation with unseen filter"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       (kodi-remote-get (a b)
			((:input '("Files.GetSources"
				   (("params"
				     ("media" . "video"))))
				 :min-occur 0)
			 (:input '("VideoLibrary.GetTVShows"
			 	   (("params"
			 	     ("properties" .
			 	      [title watchedepisodes
			 		     episode file])
			 	     ("filter" ("field" . "playcount")
				      ("operator" . "is")
				      ("value" . "0")))))
				 :min-occur 0))))
    (let* ((kodi-properties (kodi-series-response-data))
	   (kodi-watch-filter "unseen")
	   (kodi-show-df nil))
      (kodi-remote-draw-shows)
      (should (equal mode-name "kodi-remote-series: unseen"))
      (should (equal tabulated-list-format
		     '[("entries" 10 t)
		       ("Series" 30 t)]))
      (should (equal tabulated-list-entries
		     '((2 [("2" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("Series 2" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)])))))))


(ert-deftest test-draw-movies ()
  "Tests Movies Buffer generation"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       (kodi-remote-get (a b)
			:ordered nil
			((:input '("Files.GetSources"
				   (("params"
				     ("media" . "video"))))
				 :min-occur 0)
			 (:input '("VideoLibrary.GetMovies"
			 	   (("params"
			 	     ("properties" .
			 	      [title playcount resume file]))))
				 :min-occur 0))))
    (let* ((kodi-properties (kodi-movies-response-data))
	   (kodi-watch-filter "all")(kodi-show-df nil))
      (kodi-remote-draw-movies)
      (should (equal mode-name "kodi-remote-movies: all"))
      (should (equal tabulated-list-format
		     '[("Movies" 30 t)]))
      (should (equal tabulated-list-entries
		     '((1 [("Movie 1" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 1 face default resume nil)])
		       (2 [("Movie 2" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 2 face default resume nil)])))))))

(ert-deftest test-draw-movies-test-show-df ()
  "Tests Movies Buffer generation with extended diskspace info"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       ;; (kodi-remote-post (a b)((:input'("VideoLibrary.Scan"
       ;; 					 nil)
       ;; 				       :min-occur 0)))
       (kodi-remote-get (a b)
			((:input '("Files.GetSources"
				   (("params"
				     ("media" . "video"))))
				 :min-occur 0)
			 (:input '("VideoLibrary.GetMovies"
				   (("params"
				     ("properties" . [title playcount resume file]))))
				 :min-occur 0)))
       (kodi-remote-get-sources (a) ((:input '("video")
					     :output '[((file . "/PATH1")
							(label . "path1"))
						       ((file . "/PATH2")
							(label . "path2"))]
					     :min-occur 0)))
       (file-in-directory-p (a b) ((:record-cls
				    'mocker-stub-record
				    :output t
				    :min-occur 0)))
       (eshell-command-result (a) :ordered nil
			      ((:input '("df \"PATH1\" -h --output=avail")
				       :output "Avail  60G"
				       :min-occur 0)
			       (:input '("du 'PATH/Movie 1' -hs")
				       :output "8.1G	PATH/Movie 2\n"
				       :min-occur 0)
			       (:input '("du 'PATH/Movie 2' -hs")
				       :output "8.1G	PATH/Movie 2\n"
				       :min-occur 0))))
    (let* ((kodi-properties (kodi-movies-response-data))
	   (kodi-watch-filter "all")
	   (kodi-show-df t)
	   (kodi-dangerous-options t))
      (kodi-remote-draw-movies)
      (should (equal mode-name "kodi-remote-movies: all"))
      (should (equal tabulated-list-format
		     '[("Disk Free" 10 t)
		       ("Disk Used" 10 t)
		       ("Movies" 30 t)]))
      (should (equal tabulated-list-entries
		     '((1 [("60G" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 1 face default resume nil)
			   ("8.1G" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 1 face default resume nil)
			   ("Movie 1" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 1 face default resume nil)])
		       (2 [("60G" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 2 face default resume nil)
			   ("8.1G" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 2 face default resume nil)
			   ("Movie 2" action
			    #[128 "\302\300\303\301\"\"\207"
				  [sbit-action (movieid) apply append]
				  6 "\n\n(fn &rest ARGS2)"]
			    id 2 face default resume nil)])))))))


(ert-deftest test-draw-series-show-df ()
  "Tests Series Buffer generation with extended diskspace info"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       ;; (kodi-remote-post (a b)((:input'("VideoLibrary.Scan"
       ;; 					 nil)
       ;; 				       :min-occur 0)))
       (kodi-remote-get (a b) ((:input '("Files.GetSources"
					 (("params"
					   ("media" . "video"))))
				       :min-occur 0)
			       (:input '("VideoLibrary.GetTVShows"
					 (("params"
					   ("properties" .
					    [title watchedepisodes
						   episode file])
					   )))
				       :min-occur 0)
			       (:input '("VideoLibrary.GetTVShows"
					 (("params"
					   ("properties" .
					    [title watchedepisodes
						   episode file])
					   ("filter" ("field" . "playcount")
					    ("operator" . "(insert )s")
					    ("value" . "0")))))
				       :min-occur 0)))
       (kodi-remote-get-sources (a) ((:input '("video")
					     :output '[((file . "/PATH/")
							(label . "path"))
						       ((file . "/PATH2/")
							(label . "path2"))]
					     :min-occur 0)))
       (file-in-directory-p (a b) ((:record-cls 'mocker-stub-record
						:output t
						:min-occur 0)))
       (eshell-command-result (a) :ordered nil
			      ((:input '("df \"PATH/\" -h --output=avail")
				       :output "Avail  60G"
				       :min-occur 0)
			       (:input '("du 'PATH/SERIES1' -hs")
				       :output "8.1G	PATH/SERIES1\n"
				       :min-occur 0)
			       (:input '("du 'PATH/SERIES2' -hs")
				       :output "8.1G	PATH/SERIES2\n"
				       :min-occur 0))))
    (let* ((kodi-properties (kodi-series-response-data))
	   (kodi-watch-filter "all")
	   (kodi-show-df t)
	   (kodi-dangerous-options t))
      (kodi-remote-draw-shows)
      (should (equal mode-name "kodi-remote-series: all"))
      (should (equal tabulated-list-format
		     '[("entries" 10 t)
		       ("Disk Free" 10 t)
		       ("Disk Used" 10 t)
		       ("Series" 30 t)]))
      (should (equal tabulated-list-entries
		     '((1 [("1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)
			   ("60G" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)
			   ("8.1G" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)
			   ("Series 1" action kodi-remote-series-episodes-wrapper
			    id 1 face default resume nil)])
		       (2 [("7" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("60G" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("8.1G" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)
			   ("Series 2" action kodi-remote-series-episodes-wrapper
			    id 2 face default resume nil)])))))))

(ert-deftest test-draw-episodes-show-df ()
  "Tests Series Episode Buffer generation with extended diskspace info"
  (mocker-let
      ((tabulated-list-print () ((:input nil :min-occur 0)))
       ;; (kodi-remote-post (a b)((:input'("VideoLibrary.Scan"
       ;; 					 nil)
       ;; 				       :min-occur 0)))
       (kodi-remote-get (a b) ((:input '("Files.GetSources"
					 (("params"
					   ("media" . "video"))))
				       :min-occur 0)
			       (:input '("VideoLibrary.GetEpisodes"
					 (("params"
					   ("tvshowid" . 10)
					   ("properties" .
					    [title episode playcount resume
						   file]))))
				       :min-occur 0)
			       (:input '("VideoLibrary.GetEpisodes"
					 (("params"
					   ("properties" .
					    [title episode playcount resume
						   file]))))
				       :min-occur 0)))
       (kodi-remote-get-sources (a) ((:input '("video")
					     :output '[((file . "/PATH1")
							(label . "path1"))
						       ((file . "/PATH2")
							(label . "path2"))]
					     :min-occur 0)))
       (file-in-directory-p (a b) ((:record-cls 'mocker-stub-record
						:output t
						:min-occur 0)))
       (eshell-command-result (a) :ordered nil
			      ((:input '("df \"PATH1\" -h --output=avail")
				       :output "Avail  60G"
				       :min-occur 0)
			       (:input
				'("du 'PATH/Foo/Foo.S01E01.mkv' -hs")
				:output "8.1G	PATH/Foo/Foo.S01E01.mkv\n"
				:min-occur 0)
			       (:input
				'("du 'PATH/Foo/Foo.S01E02.mkv' -hs")
				:output "8.1G	PATH/Foo/Foo.S01E02.mkv\n"
				:min-occur 0))))
    (let* ((kodi-properties (kodi-episodes-response-data))
	   (kodi-watch-filter "all")
	   (kodi-show-df t)
	   (kodi-dangerous-options t))
      (kodi-remote-draw-episodes)
      (should (equal mode-name "kodi-remote-series-episodes: all"))
      (should (equal tabulated-list-format
		     '[("Disk Free" 10 t)
		       ("Disk Used" 10 t)
		       ("Episode" 30 t)]))
      (should (equal tabulated-list-entries
		     '((10 [("60G" action
			     #[128 "\302\300\303\301\"\"\207"
				   [sbit-action (episodeid) apply append]
				   6 "\n\n(fn &rest ARGS2)"]
			     id 10 face default resume nil)
			    ("8.1G" action
			     #[128 "\302\300\303\301\"\"\207"
				   [sbit-action (episodeid) apply append]
				   6 "\n\n(fn &rest ARGS2)"]
			     id 10 face default resume nil)
			    ("1x01. Title 1" action
			     #[128 "\302\300\303\301\"\"\207"
				   [sbit-action (episodeid) apply append]
				   6 "\n\n(fn &rest ARGS2)"]
			     id 10 face default resume nil)])
		       (20 [("60G" action #[128 "\302\300\303\301\"\"\207"
						[sbit-action (episodeid) apply append]
						6 "\n\n(fn &rest ARGS2)"]
			     id 20 face default resume nil)
			    ("8.1G" action #[128 "\302\300\303\301\"\"\207"
						 [sbit-action (episodeid) apply append]
						 6 "\n\n(fn &rest ARGS2)"]
			     id 20 face default resume nil)
			    ("1x02. Title 2" action #[128 "\302\300\303\301\"\"\207"
							  [sbit-action (episodeid) apply append]
							  6 "\n\n(fn &rest ARGS2)"]
			     id 20 face default resume nil)])))))))
