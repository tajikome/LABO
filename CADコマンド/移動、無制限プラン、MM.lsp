(defun c:MM ( / ss pt1 pt2 pt_list dist_list last_dist)
  (prompt "\n連続移動（U入力で1回分戻る）")
  (if (setq ss (ssget))
    (progn
      (if (setq pt1 (getpoint "\n1回目の基点を指定: "))
        (progn
          ;; 履歴の初期化
          (setq pt_list (list pt1))
          (setq dist_list nil)

          ;; 入力が空（Enter）になるまでループ
          (while (progn
                   (initget "Undo") ; 'U' または 'Undo' キーワードを有効化
                   (setq pt2 (getpoint (car pt_list) "\n目的点を指定 [戻る(U)] : "))
                 )

            (if (= pt2 "Undo")
              ;; --- Undo（戻る）処理 ---
              (if (> (length pt_list) 1)
                (progn
                  (command "._UNDO" "1") ; CADの操作を1つ戻す
                  (setq pt_list (cdr pt_list)) ; 直前の点を削除
                  (setq dist_list (cdr dist_list)) ; 直前の入力距離を削除
                  (prompt "\n一つ前の位置に戻りました。")
                )
                (prompt "\nこれ以上は戻れません。")
              )
              ;; --- 通常の移動処理 ---
              (progn
                ;; 移動前の点と目的点の距離を取得して記録
                (setq last_dist (distance (car pt_list) pt2))
                (setq dist_list (cons last_dist dist_list))

                (command "._MOVE" ss "" "_non" (car pt_list) "_non" pt2)
                (setq pt_list (cons pt2 pt_list)) ; 新しい目的点を履歴に追加

                ;; ★ 1回移動するごとに、直前に入力した数字（距離）を表示 ★
                (prompt (strcat "\n入力値: " (rtos last_dist)))
              )
            )
          )
        )
      )
    )
  )
  (princ)
)