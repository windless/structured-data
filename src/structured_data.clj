(ns structured-data)

;; (def china {:name "China Mieville" :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman"
;;                :birth-year 1944})
;; (def felleisen {:name "Matthias felleisen"})
;;
;; (def cities {:title "The City and the City" :authors [china]})
;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
;; (def embassytown {:title "Embassytown", :authors [china]})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})
;;
;; (def books [cities wild-seed embassytown little-schemer])

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[p1 p2] point]
      (and (<= x1 p1 x2)
           (<= y1 p2 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-authors (or (:authors book) [])]
    (let [new-authors (conj new-authors new-author)]
      (let [title (:title book)]
        (if title {:title title :authors new-authors} {:authors new-authors})))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (let [sets-all (fn [x] (set (:authors x)))]
    (apply clojure.set/union (map sets-all books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [[name birth-year death-year] [(:name author) (:birth-year author) (:death-year author)]]
    (if birth-year
      (str name " (" birth-year " - " death-year ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (= 0 book-count) "No books."
      (= 1 book-count) (str "1 book. " (book->string (get books 0)) ".")
      :else (str book-count " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
