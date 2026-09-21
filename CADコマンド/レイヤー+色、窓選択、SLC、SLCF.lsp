;; =========================================================================
;; 共通ヘルパー関数群
;; =========================================================================

;; オブジェクトの色番号を取得 (ByLayerの場合は画層の色を取得)
(defun @ss-get-color (ent-data lay / color-info)
  (setq color-info (assoc 62 ent-data))
  (if color-info 
    (cdr color-info) 
    (abs (cdr (assoc 62 (tblsearch "LAYER" lay))))
  )
)

;; 色番号から色名称の文字列を取得
(defun @ss-get-color-name (c)
  (cond 
    ((= c 1) "赤(R)")
    ((= c 2) "黄(Y)")
    ((= c 3) "緑(G)")
    ((= c 4) "水色(C)")
    ((= c 5) "青(B)")
    ((= c 6) "紫(M)")
    ((= c 7) "白黒(W)")
    (t (strcat "色番" (itoa c)))
  )
)

;; 文字列を区切り文字(カンマやスペース)で分割
(defun @ss-parse-delim (str / idx char cur lst)
  (setq idx 1 cur "" lst nil)
  (while (<= idx (strlen str))
    (setq char (substr str idx 1))
    (if (or (= char ",") (= char " "))
      (progn
        (if (/= cur "") (setq lst (cons cur lst)))
        (setq cur "")
      )
      (setq cur (strcat cur char))
    )
    (setq idx (1+ idx))
  )
  (if (/= cur "") (setq lst (cons cur lst)))
  (reverse lst)
)

;; 文字列リストを色番号リストに変換
(defun @ss-get-color-codes (str-list / code lst)
  (setq lst nil)
  (foreach s str-list
    (setq s (strcase s))
    (cond
      ((= s "R") (setq code 1))
      ((= s "Y") (setq code 2))
      ((= s "G") (setq code 3))
      ((= s "C") (setq code 4))
      ((= s "B") (setq code 5))
      ((= s "M") (setq code 6))
      ((= s "W") (setq code 7))
      ((numberp (read s))
       (setq code (atoi s))
       (if (or (< code 0) (> code 256)) (setq code nil))
      )
      (t (setq code nil))
    )
    (if (and code (not (member code lst)))
      (setq lst (cons code lst))
    )
  )
  (reverse lst)
)


;; =========================================================================
;; コマンド1: SLC (画層と色をペアで一発抽出)
;; =========================================================================
(defun c:SLC (/ *error* old-err ss-source i ent ent-data lay col-val pair lay-col-list msg c-str item pt1 pt2 ss-result idx ss-manual-remove ss-temp j rem-ent ss-valid ss-remove ss-filtered keep mode post-ss)
  
  ;; --- エラーハンドリング設定 ---
  (setq old-err *error*)
  (defun *error* (msg)
    (if (not (member msg '("関数がキャンセルされました" "quit / exit abort")))
      (princ (strcat "\nエラー: " msg))
    )
    (if ss-result
      (progn
        (setq idx 0)
        (while (< idx (sslength ss-result))
          (redraw (ssname ss-result idx) 4)
          (setq idx (1+ idx))
        )
      )
    )
    (command "._UNDO" "_E")
    (setq *error* old-err)
    (princ)
  )

  ;; Undoグループ開始
  (command "._UNDO" "_BE")
  (setq post-ss nil)

  ;; コマンド説明文
  (princ "\n=== [SLC] 画層と色を同時に一致させてオブジェクトを一括抽出します ===")

  ;; 1. 元となる「画層と色」を取得するためのオブジェクト選択
  (princ "\n抽出したい「画層と色」を持つオブジェクトを選択（複数選択可）: ")
  (if (setq ss-source (ssget))
    (progn
      (setq i 0 lay-col-list nil)
      (while (< i (sslength ss-source))
        (setq ent (ssname ss-source i))
        (setq ent-data (entget ent))
        (setq lay (cdr (assoc 8 ent-data)))
        (setq col-val (@ss-get-color ent-data lay))
        
        (setq pair (cons lay col-val))
        (if (not (member pair lay-col-list))
          (setq lay-col-list (cons pair lay-col-list))
        )
        (setq i (1+ i))
      )
      
      (setq msg "")
      (foreach pair lay-col-list
        (setq c-str (@ss-get-color-name (cdr pair)))
        (setq item (strcat (car pair) "[" c-str "]"))
        (if (= msg "") (setq msg item) (setq msg (strcat msg " , " item)))
      )
      (princ (strcat "\n抽出条件(画層[色]): " msg))
      
      ;; 2. 対象範囲を2点で指定
      (princ "\n対象とする範囲を交差窓の2点で指定してください...")
      (setq pt1 (getpoint "\n1つ目の角を指定: "))
      (if pt1 (setq pt2 (getcorner pt1 "\n対角の角を指定: ")))
      
      (if (and pt1 pt2)
        (if (setq ss-result (ssget "_C" pt1 pt2))
          (progn
            (setq idx 0)
            (while (< idx (sslength ss-result))
              (redraw (ssname ss-result idx) 3)
              (setq idx (1+ idx))
            )

            (setq ss-manual-remove (ssadd))
            (princ "\n【追加操作】除外したいオブジェクトをクリック (再クリックで復活 / 完了はEnter): ")
            (while (setq ss-temp (ssget "_:S"))
              (setq j 0)
              (while (< j (sslength ss-temp))
                (setq rem-ent (ssname ss-temp j))
                (if (ssmemb rem-ent ss-result)
                  (if (ssmemb rem-ent ss-manual-remove)
                    (progn
                      (ssdel rem-ent ss-manual-remove)
                      (redraw rem-ent 3)
                    )
                    (progn
                      (redraw rem-ent 4)
                      (ssadd rem-ent ss-manual-remove)
                    )
                  )
                )
                (setq j (1+ j))
              )
            )
            
            (setq idx 0)
            (while (< idx (sslength ss-result))
              (redraw (ssname ss-result idx) 4)
              (setq idx (1+ idx))
            )

            (setq ss-valid (ssadd) ss-remove (ssadd) idx 0)
            (while (< idx (sslength ss-result))
              (setq ent (ssname ss-result idx))
              (if (ssmemb ent ss-manual-remove)
                (ssadd ent ss-remove)
                (ssadd ent ss-valid)
              )
              (setq idx (1+ idx))
            )
            (setq ss-result ss-valid)

            (setq ss-filtered (ssadd) i 0)
            (while (< i (sslength ss-result))
              (setq ent (ssname ss-result i))
              (setq ent-data (entget ent))
              (setq lay (cdr (assoc 8 ent-data)))
              (setq col-val (@ss-get-color ent-data lay))
              
              (setq keep nil)
              (if (member (cons lay col-val) lay-col-list)
                (setq keep t)
              )
              
              (if keep (ssadd ent ss-filtered) (ssadd ent ss-remove))
              (setq i (1+ i))
            )
            
            (if (> (sslength ss-filtered) 0)
              (progn
                (setq mode (getstring "\n処理を選択 [ストレッチ(S) / Enter(通常選択)] <通常選択>: "))
                
                (if (= (strcase mode) "S")
                  (progn
                    (if (> (sslength ss-remove) 0)
                      (command "._stretch" "_C" pt1 pt2 "_R" ss-remove "")
                      (command "._stretch" "_C" pt1 pt2 "")
                    )
                    (while (> (getvar "CMDACTIVE") 0) (command pause))
                  )
                  (setq post-ss ss-filtered)
                )
              )
              (princ "\n指定範囲内に該当するオブジェクトはありませんでした。")
            )
          )
          (princ "\n除外後、処理対象のオブジェクトがなくなりました。")
        )
        (princ "\n指定範囲内にオブジェクトはありませんでした。")
      )
    )
    (princ "\n範囲指定がキャンセルされました。")
  )
  
  ;; 終了処理
  (command "._UNDO" "_E")
  (setq *error* old-err)

  (if post-ss
    (progn
      (sssetfirst nil post-ss)
      (princ (strcat "\n" (rtos (sslength post-ss) 2 0) "個のオブジェクトを選択しました。"))
    )
  )
  (princ)
)


;; =========================================================================
;; コマンド2: SLCF (画層抽出後、範囲内の色からフィルタ選択)
;; =========================================================================
(defun c:SLCF (/ *error* old-err ss-source i ent ent-data lay lay-list lay-str l pt1 pt2 ss-result idx ss-manual-remove ss-temp j rem-ent ss-valid ss-remove color-list col-val col-str c-str col-input col-codes ss-filtered keep mode post-ss)
  
  ;; --- エラーハンドリング設定 ---
  (setq old-err *error*)
  (defun *error* (msg)
    (if (not (member msg '("関数がキャンセルされました" "quit / exit abort")))
      (princ (strcat "\nエラー: " msg))
    )
    (if ss-result
      (progn
        (setq idx 0)
        (while (< idx (sslength ss-result))
          (redraw (ssname ss-result idx) 4)
          (setq idx (1+ idx))
        )
      )
    )
    (command "._UNDO" "_E")
    (setq *error* old-err)
    (princ)
  )

  ;; Undoグループ開始
  (command "._UNDO" "_BE")
  (setq post-ss nil)

  ;; コマンド説明文
  (princ "\n=== [SLCF] 指定画層を抽出後、範囲内の色を一覧から選んで絞り込みます ===")

  ;; 1. 元となる「画層」を取得するためのオブジェクト選択
  (princ "\n抽出したい画層にあるオブジェクトを選択（どれか1つでOK）: ")
  (if (setq ss-source (ssget))
    (progn
      (setq i 0 lay-list nil)
      (while (< i (sslength ss-source))
        (setq ent (ssname ss-source i))
        (setq ent-data (entget ent))
        (setq lay (cdr (assoc 8 ent-data)))
        (if (not (member lay lay-list)) (setq lay-list (cons lay lay-list)))
        (setq i (1+ i))
      )
      
      (setq lay-str "")
      (foreach l lay-list
        (if (= lay-str "") (setq lay-str l) (setq lay-str (strcat lay-str "," l)))
      )
      (princ (strcat "\n選択された画層: [" lay-str "]"))
      
      ;; 2. 対象範囲を2点で指定
      (princ "\n対象とする範囲を交差窓の2点で指定してください...")
      (setq pt1 (getpoint "\n1つ目の角を指定: "))
      (if pt1 (setq pt2 (getcorner pt1 "\n対角の角を指定: ")))
      
      (if (and pt1 pt2)
        (if (setq ss-result (ssget "_C" pt1 pt2))
          (progn
            (setq idx 0)
            (while (< idx (sslength ss-result))
              (redraw (ssname ss-result idx) 3)
              (setq idx (1+ idx))
            )

            (setq ss-manual-remove (ssadd))
            (princ "\n【追加操作】除外したいオブジェクトをクリック (再クリックで復活/完了はEnter): ")
            (while (setq ss-temp (ssget "_:S"))
              (setq j 0)
              (while (< j (sslength ss-temp))
                (setq rem-ent (ssname ss-temp j))
                (if (ssmemb rem-ent ss-result)
                  (if (ssmemb rem-ent ss-manual-remove)
                    (progn
                      (ssdel rem-ent ss-manual-remove)
                      (redraw rem-ent 3)
                    )
                    (progn
                      (redraw rem-ent 4)
                      (ssadd rem-ent ss-manual-remove)
                    )
                  )
                )
                (setq j (1+ j))
              )
            )
            
            (setq idx 0)
            (while (< idx (sslength ss-result))
              (redraw (ssname ss-result idx) 4)
              (setq idx (1+ idx))
            )

            (setq ss-valid (ssadd) ss-remove (ssadd) idx 0)
            (while (< idx (sslength ss-result))
              (setq ent (ssname ss-result idx))
              (if (ssmemb ent ss-manual-remove)
                (ssadd ent ss-remove)
                (ssadd ent ss-valid)
              )
              (setq idx (1+ idx))
            )
            (setq ss-result ss-valid)

            (if (> (sslength ss-result) 0)
              (progn
                (setq i 0 color-list nil)
                (while (< i (sslength ss-result))
                  (setq ent (ssname ss-result i))
                  (setq ent-data (entget ent))
                  (setq lay (cdr (assoc 8 ent-data)))
                  
                  (if (member lay lay-list)
                    (progn
                      (setq col-val (@ss-get-color ent-data lay))
                      (if (not (member col-val color-list)) (setq color-list (cons col-val color-list)))
                    )
                  )
                  (setq i (1+ i))
                )

                (setq col-str "")
                (foreach c color-list
                  (setq c-str (@ss-get-color-name c))
                  (if (= col-str "") (setq col-str c-str) (setq col-str (strcat col-str "/" c-str)))
                )

                (if (> (length color-list) 0)
                  (progn
                    (princ "\n---色フィルター---")
                    (princ "\n赤(R)/黄(Y)/緑(G)/水色(C)/青(B)/紫(M)/白黒(W)")
                    (princ "\n※複数指定時はカンマ(,)で区切る (例: R,Y)")
                    (princ (strcat "\n範囲内の対象画層の色: [" col-str "]"))
                    (princ "\n--------------------")
                    
                    (setq col-input (getstring "\n抽出する色を選択 [カンマ区切り入力] <すべて>: "))
                    (if (or (= col-input "") (= (strcase col-input) "A"))
                      (setq col-codes nil)
                      (setq col-codes (@ss-get-color-codes (@ss-parse-delim col-input)))
                    )
                  )
                  (setq col-codes nil)
                )
                
                (setq ss-filtered (ssadd) i 0)
                (while (< i (sslength ss-result))
                  (setq ent (ssname ss-result i))
                  (setq ent-data (entget ent))
                  (setq lay (cdr (assoc 8 ent-data)))
                  (setq col-val (@ss-get-color ent-data lay))
                  
                  (setq keep nil)
                  (if (member lay lay-list)
                    (if (or (not col-codes) (member col-val col-codes)) (setq keep t))
                  )
                  (if keep (ssadd ent ss-filtered) (ssadd ent ss-remove))
                  (setq i (1+ i))
                )
                
                (if (> (sslength ss-filtered) 0)
                  (progn
                    (setq mode (getstring "\n処理を選択 [ストレッチ(S)/Enter(通常選択)] <通常選択>: "))
                    
                    (if (= (strcase mode) "S")
                      (progn
                        (if (> (sslength ss-remove) 0)
                          (command "._stretch" "_C" pt1 pt2 "_R" ss-remove "")
                          (command "._stretch" "_C" pt1 pt2 "")
                        )
                        (while (> (getvar "CMDACTIVE") 0) (command pause))
                      )
                      (setq post-ss ss-filtered)
                    )
                  )
                  (princ "\n指定範囲内に該当するオブジェクトはありませんでした。")
                )
              )
              (princ "\n除外後、処理対象のオブジェクトがなくなりました。")
            )
          )
          (princ "\n指定範囲内にオブジェクトはありませんでした。")
        )
      )
      (princ "\n範囲指定がキャンセルされました。")
    )
    (princ "\n最初のオブジェクト選択がキャンセルされました。")
  )
  
  ;; 終了処理
  (command "._UNDO" "_E")
  (setq *error* old-err)

  (if post-ss
    (progn
      (sssetfirst nil post-ss)
      (princ (strcat "\n" (rtos (sslength post-ss) 2 0) "個のオブジェクトを選択しました。"))
    )
  )
  (princ)
)