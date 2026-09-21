
(defun c:MM ( / ss pt1 pt2 pt_list last_pt)
  (prompt "\n連続移動（U入力で1回分戻る）")
  (if (setq ss (ssget))
    (progn
      (if (setq pt1 (getpoint "\n1回目の基点を指定: "))
        (progn
          ;; 基点の履歴を管理するリスト
          (setq pt_list (list pt1))

          ;; 入力が空（Enter）になるまでループ
          (while (progn
                   (initget "Undo") ; 'U' または 'Undo' キーワードを有効化
                   (setq pt2 (getpoint (car pt_list) "\n目的点を指定 [戻る(U)] <終了>: "))
                 )

            (if (= pt2 "Undo")
              ;; --- Undo（戻る）処理 ---
              (if (> (length pt_list) 1)
                (progn
                  (command "._UNDO" "1") ; CADの操作を1つ戻す
                  (setq pt_list (cdr pt_list)) ; リストの先頭（最新の点）を削除
                  (prompt "\n一つ前の位置に戻りました。")
                )
                (prompt "\nこれ以上は戻れません。")
              )
              ;; --- 通常の移動処理 ---
              (progn
                (command "._MOVE" ss "" "_non" (car pt_list) "_non" pt2)
                (setq pt_list (cons pt2 pt_list)) ; 新しい目的点を履歴の先頭に追加
              )
            )
          )
        )
      )
    )
  )
  (princ)
)