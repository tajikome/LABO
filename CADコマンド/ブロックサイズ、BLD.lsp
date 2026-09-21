(defun c:BLD ( / opt ent ptList sset i e blkRefName dim w h has-x has-y blkDef entDef pt1 pt2 dx dy uniqueBlks cnt j final-ss targetSize match bw bh)
  (vl-load-com)
  
  ;; 選択方法の指定（窓選択または境界選択）
  (initget "Window Boundary")
  (setq opt (getkword "\nブロックの選択方法を指定 [窓・通常選択(W)/閉じたオブジェクト境界(B)] : "))
  
  (setq sset nil)
  (if (= opt "Boundary")
    (progn
      (setq ent (car (entsel "\n内側にあるブロックを抽出する閉じたオブジェクト（ポリライン等）を選択: ")))
      (if ent
        (progn
          (setq ptList nil)
          (foreach x (entget ent)
            (if (= (car x) 10)
              (setq ptList (append ptList (list (cdr x))))
            )
          )
          (if ptList
            (setq sset (ssget "CP" ptList '((0 . "INSERT"))))
          )
        )
      )
    )
    (progn
      (princ "\n対象のブロックを窓選択またはクリックで選択してください:")
      (setq sset (ssget '((0 . "INSERT"))))
    )
  )
  
  ;; 選択されたブロックがない場合は終了
  (if (or (null sset) (= (sslength sset) 0))
    (progn
      (princ "\n\nブロックが選択されませんでした。")
      (exit)
    )
  )

  ;; 選択されたブロックをハイライト表示
  (sssetfirst nil sset)

  ;; ブロック定義から直接の寸法（最小・最大座標）を計算するローカル関数
  (defun get-block-def-extents (bName / bObj subEnt minPt maxPt p1 p2)
    (setq bObj (tblobjname "BLOCK" bName))
    (setq minPt nil maxPt nil)
    (while bObj
      (setq subEnt (entget bObj))
      (if (not (vl-position '(0 . "ATTRIB") subEnt))
        (progn
          (cond
            ((= (cdr (assoc 0 subEnt)) "LINE")
             (setq p1 (cdr (assoc 10 subEnt)))
             (setq p2 (cdr (assoc 11 subEnt)))
             (setq minPt (if minPt (mapcar 'min minPt p1) p1))
             (setq maxPt (if maxPt (mapcar 'max maxPt p1) p1))
             (setq minPt (mapcar 'min minPt p2))
             (setq maxPt (mapcar 'max maxPt p2))
            )
            ((= (cdr (assoc 0 subEnt)) "LWPOLYLINE")
             (foreach pt subEnt
               (if (= (car pt) 10)
                 (progn
                   (setq p1 (list (cadr pt) (caddr pt) 0.0))
                   (setq minPt (if minPt (mapcar 'min minPt p1) p1))
                   (setq maxPt (if maxPt (mapcar 'max maxPt p1) p1))
                 )
               )
             )
            )
          )
        )
      )
      (setq bObj (entnext bObj))
    )
    (if (and minPt maxPt)
      (list (- (car maxPt) (car minPt)) (- (cadr maxPt) (cadr minPt)))
      '(0.0 0.0)
    )
  )

  ;; --- 計測結果の集計とサイズ別リストの作成 ---
  (setq uniqueBlks '())
  (setq i 0)
  (while (< i (sslength sset))
    (setq blkRefName (cdr (assoc 2 (entget (ssname sset i)))))
    (if (not (member blkRefName uniqueBlks))
      (setq uniqueBlks (cons blkRefName uniqueBlks))
    )
    (setq i (1+ i))
  )
  
  (princ "\n\n=== 【選択したブロックの計測結果一覧】 ===")
  (foreach blkRefName uniqueBlks
    (setq blkDef (tblobjname "BLOCK" blkRefName))
    (setq has-x nil has-y nil)
    (while blkDef
      (setq entDef (entget blkDef))
      (if (= (cdr (assoc 0 entDef)) "LINE")
        (progn
          (setq pt1 (cdr (assoc 10 entDef)))
          (setq pt2 (cdr (assoc 11 entDef)))
          (setq dx (abs (- (car pt2) (car pt1))))
          (setq dy (abs (- (cadr pt2) (cadr pt1))))
          (if (and (< dy 0.001) (< (abs (- dx 100.0)) 0.001)) (setq has-x T))
          (if (and (< dx 0.001) (< (abs (- dy 100.0)) 0.001)) (setq has-y T))
        )
      )
      (setq blkDef (entnext blkDef))
    )
    
    (setq dim (get-block-def-extents blkRefName))
    (setq w (car dim))
    (if (or has-x has-y)
      (setq h (- (cadr dim) 50.0))
      (setq h (cadr dim))
    )
    
    ;; 個数の集計
    (setq j 0 cnt 0)
    (while (< j (sslength sset))
      (if (= (cdr (assoc 2 (entget (ssname sset j)))) blkRefName)
        (setq cnt (1+ cnt))
      )
      (setq j (1+ j))
    )
    
    (princ (strcat "\n  ・[ブロック名: " blkRefName "] 幅=" (rtos w 2 2) " / 高さ=" (rtos h 2 2) " (個数: " (itoa cnt) "個)"))
  )

  ;; --- 計測後にサイズで選び直す（フィルタリング）機能 ---
  (initget "All Specific")
  (setq targetSize (getkword "\n\n計測完了。どのように選択しますか？ [すべて維持(A) / 特定のサイズだけ絞り込む(S)] : "))
  
  (if (= targetSize "Specific")
    (progn
      (princ "\n※ 絞り込みたい項目のみ数値を入力してください。指定しない項目はそのままEnterでスキップできます。")
      (setq w (getreal "\n絞り込む「幅」を入力 [スキップはEnter]: "))
      (setq h (getreal "\n絞り込む「高さ」を入力 [スキップはEnter]: "))
      
      ;; 両方未入力の場合は警告を出して終了
      (if (and (null w) (null h))
        (progn
          (princ "\n幅も高さも入力されなかったため、絞り込みをキャンセルし選択を維持します。")
          (sssetfirst nil sset)
        )
        (progn
          (setq final-ss (ssadd))
          (setq j 0)
          (while (< j (sslength sset))
            (setq e (ssname sset j))
            (setq blkRefName (cdr (assoc 2 (entget e))))
            
            ;; 再度サイズを判定
            (setq blkDef (tblobjname "BLOCK" blkRefName))
            (setq has-x nil has-y nil)
            (while blkDef
              (setq entDef (entget blkDef))
              (if (= (cdr (assoc 0 entDef)) "LINE")
                (progn
                  (setq pt1 (cdr (assoc 10 entDef)))
                  (setq pt2 (cdr (assoc 11 entDef)))
                  (setq dx (abs (- (car pt2) (car pt1))))
                  (setq dy (abs (- (cadr pt2) (cadr pt1))))
                  (if (and (< dy 0.001) (< (abs (- dx 100.0)) 0.001)) (setq has-x T))
                  (if (and (< dx 0.001) (< (abs (- dy 100.0)) 0.001)) (setq has-y T))
                )
              )
              (setq blkDef (entnext blkDef))
            )
            (setq dim (get-block-def-extents blkRefName))
            (setq bw (car dim))
            (setq bh (if (or has-x has-y) (- (cadr dim) 50.0) (cadr dim)))
            
            ;; 判定フラグ
            (setq match T)
            ;; 幅が入力されている場合、幅が一致するかチェック
            (if (and w (not (< (abs (- bw w)) 0.001)))
              (setq match nil)
            )
            ;; 高さが入力されている場合、高さが一致するかチェック
            (if (and h (not (< (abs (- bh h)) 0.001)))
              (setq match nil)
            )
            
            ;; 条件に合致すれば追加
            (if match
              (ssadd e final-ss)
            )
            
            (setq j (1+ j))
          )
          
          (if (> (sslength final-ss) 0)
            (progn
              (sssetfirst nil final-ss)
              (princ (strcat "\n--- 指定された条件に一致する " (itoa (sslength final-ss)) " 個のブロックを選択しました ---"))
            )
            (progn
              (sssetfirst nil nil)
              (princ "\n--- 指定された条件に一致するブロックはありませんでした ---")
            )
          )
        )
      )
    )
    ;; すべて維持する場合
    (progn
      (sssetfirst nil sset)
      (princ (strcat "\n--- 処理完了: 合計 " (itoa (sslength sset)) " 個のブロックを選択状態にしています ---"))
    )
  )
  (princ)
)