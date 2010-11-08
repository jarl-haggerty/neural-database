(ns com.curiouscat.neural-database.core
  (:use clojure.contrib.sql
        clojure.contrib.swing-utils
        clojure.java.io)
  (:import java.io.File
           java.sql.BatchUpdateException
           java.awt.GridLayout
           java.awt.event.ActionListener
           java.awt.event.ComponentListener
           javax.swing.JFrame
           javax.swing.JPanel
           javax.swing.JLabel
           javax.swing.JPasswordField
           javax.swing.JButton
           org.jfree.data.xy.XYSeries
           org.jfree.data.xy.XYSeriesCollection
           org.jfree.chart.ChartFactory
           org.jfree.chart.plot.PlotOrientation
           org.jfree.chart.ChartPanel
           org.jfree.chart.axis.NumberAxis
           org.jfree.chart.plot.XYPlot
           org.jfree.chart.renderer.xy.XYErrorRenderer
           org.jfree.chart.JFreeChart
           org.jfree.data.xy.YIntervalSeries
           org.jfree.data.xy.YIntervalSeriesCollection
           org.jfree.chart.title.TextTitle))

(def db
     {:classname "com.mysql.jdbc.Driver" :subprotocol "mysql" :subname "//localhost:3306/curare_trials" :user "root"})

(def password (atom nil))
(defn chars-to-string [#^chars coll] (String. coll))
(defn get-password []
  (let [password-field (JPasswordField.)
        cancel-button (JButton. "Cancel")
        ok-button (JButton. "Ok")
        panel (doto (JPanel.)
                (.setLayout (GridLayout. 2 2))
                (.add (JLabel. "Enter Password:"))
                (.add password-field)
                (.add cancel-button)
                (.add ok-button))
        frame (doto (JFrame. "Password Needed")
                (.setContentPane panel)
                (.pack)
                (.setLocationRelativeTo nil)
                (.setVisible true))]
    (swap! password (fn [x] nil))
    (.addComponentListener frame
                           (proxy [ComponentListener] []
                             (componentHidden [_]
                                              (if (not @password) (swap! password (fn [x] (char-array 0))))
                                              (.setVisible frame false))))
    (.addActionListener password-field
                         (proxy [ActionListener] []
                           (actionPerformed [_]
                                           (swap! password (fn [x] (.getPassword password-field)))
                                           (.setVisible frame false))))
    (.addActionListener ok-button
                        (proxy [ActionListener] []
                          (actionPerformed [_]
                                           (swap! password (fn [x] (.getPassword password-field)))
                                           (.setVisible frame false))))
    (.addActionListener cancel-button
                        (proxy [ActionListener] []
                          (actionPerformed [_]
                                           (swap! password (fn [x] (char-array 0)))
                                           (.setVisible frame false))))
    (while (not @password))
    (swap! password chars-to-string))
  nil)


(def phases ["Control" "60_uM_Acetylcholine" "60_uM_Acetylcholine_and_10_uM_Curare" "Washout"])

(defn clean-string [x] (-> x .trim (.replace " " "_")))

(defn checkin-file [& file-names]
  (doseq [file-name file-names]
    (if (.isDirectory (file file-name))
      (apply checkin-file (map #(str "analysis/" %) (.list (file file-name))))
      (let [name (.replace (.substring file-name (inc (.indexOf file-name File/separator))) ".csv" "")]
        (with-open [input (reader file-name)]
          (with-connection (assoc db :password @password)
            (.readLine input)
            (loop [row (map clean-string (-> input .readLine (.split ",")))]
              (when (= (count row) 3)
                (with-query-results res ["show tables"]
                  (if (not (contains? (set (flatten (map vals res))) (nth row 0)))
                    (create-table (nth row 0) [:experiment "varchar(50)"] [:start :double] [:stop :double])))
                (update-or-insert-values (nth row 0)
                                         ["experiment = ?" name]
                                         {:experiment name :start (nth row 1) :stop (nth row 2)})
                (recur (map clean-string (-> input .readLine (.split ","))))))
            (with-query-results res ["show tables"]
              (if (contains? (set (flatten (map vals res))) name)
                (drop-table name)))
            (create-table name
                          [:time :double]
                          [:area :double]
                          [:amplitude :double]
                          [:duration :double]
                          [:fractional_peak :double]
                          [:frequency  :double])
            (apply insert-rows name
                   (map #(vec (.split % ",")) (line-seq input)))))))))

(defn bin-frequency [bin-size phases & experiments]
  (with-connection (assoc db :password @password)
    (apply merge-with (fn [seq1 seq2] (map #(flatten (list %2 %1)) seq1 seq2))
           (for [experiment experiments phase phases]
             (let [[start stop] (with-query-results result
                                  [(str "SELECT start, stop FROM " phase " WHERE experiment = \"" experiment "\"")]
                                  [(:start (first result)) (:stop (first result))])
                   times (with-query-results result
                           [(str "SELECT time FROM " experiment " WHERE time > " start" AND time < " stop)]
                           (doall (map :time result)))]
               (apply merge-with #(map + %1 %2)
                      (for [time times]
                        {(keyword phase) (for [i (range (/ (- stop start) bin-size))]
                                          (if (= i (int (/ (- time start) bin-size))) 1 0))})))))))



(defn mean [& args] (/ (reduce + args) (count args)))
(defn variance [& args]
  (let [middle (apply mean args)]
    (/ (reduce + (map #(Math/pow (- middle %) 2) args)) (count args))))
(defn standard-deviation [& args] (Math/sqrt (apply variance args)))

(defn normalize-by [plot unit]
  (let [average (apply mean (map #(apply mean %) (unit plot)))]
    (apply merge (for [p plot]
                   {(first p) (for [x (second p)] (map #(/ % average) x))}))))

(defn plot-bins
  ([bin-size plots]
     (let [[min-range max-range] (let [temp (flatten (for [a (vals plots) b (vals a)] b))]
                                   [(apply min temp) (apply max temp)])
           series
           (apply merge
            (for [plot plots]
              {(first plot)
               (apply merge
                      (for [data (second plot)]
                        {(first data)
                         (let [series (YIntervalSeries. (first plot))]
                           (doseq [i (-> (second data) count range)]
                             (.add series (* i bin-size) (apply mean (nth (second data) i))
                                   (- (apply mean (nth (second data) i)) (apply standard-deviation (nth (second data) i)))
                                   (+ (apply mean (nth (second data) i)) (apply standard-deviation (nth (second data) i)))))
                           series)}))}))
           series-collections
           (loop [stack series accum {}]
             (if-let [current (first stack)]
               (do
                 (recur (rest stack)
                        (loop [stack2 (second current) accum2 accum]
                          (if-let [current2 (first stack2)]
                            (do
                              (recur (rest stack2)
                                     (assoc accum2
                                       (first current2)
                                       (doto (get accum2
                                                  (first current2)
                                                  (YIntervalSeriesCollection.))
                                         (.addSeries (second current2))))))
                            accum2))))
               accum))
           panel
           (doto (JPanel.)
             (.setLayout (GridLayout. 1 (count series-collections))))
           
           charts
           (doseq [collection series-collections]
             (let [chart (JFreeChart. (.replace (name (first collection)) "_" " ")
                                      (.deriveFont TextTitle/DEFAULT_FONT (float 13))
                                      (XYPlot. (second collection)
                                               (NumberAxis. "Time")
                                               (NumberAxis. "Voltage")
                                               (doto (XYErrorRenderer.)
                                                 (.setBaseLinesVisible true)
                                                 (.setBaseShapesVisible false)
                                                 (.setDrawXError false)
                                                 (.setDrawYError true)))
                                      true)]
               (.setLowerBound (.getRangeAxis (.getXYPlot chart)) min-range)
               (.setUpperBound (.getRangeAxis (.getXYPlot chart)) max-range)
               (.add panel (ChartPanel. chart))))
           
           frame (doto (JFrame. "Plot")
                   (.setContentPane panel)
                   (.setLocationRelativeTo nil)
                   (.setVisible true)
                   (.setExtendedState JFrame/MAXIMIZED_BOTH))])))
