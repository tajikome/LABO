
(defun c:SCOLOR (/ base colors ent color area result all i)

  (vl-load-com)

  ;; 1. 基準オブジェクトを複数選択
  (princ "\n基準色を取得するオブジェクトを選択してください: ")
  (setq base (ssget))

  (if base
    (progn

      ;; 2. 色を取得
      (setq colors nil
            i 0)

      (repeat (sslength base)
        (setq ent (ssname base i))
        (setq color (sc:get-color ent))

        (if (not (member color colors))
          (setq colors (cons color colors))
        )

        (setq i (1+ i))
      )

      ;; 3. 検索範囲を指定
      (princ "\n検索範囲を窓選択してください。Enterで図面全体: ")
      (setq area (ssget))

      (if area
        (setq all area)
        (setq all (ssget "_X"))
      )

      ;; 4. 同色オブジェクトを抽出
      (setq result (ssadd)
            i 0)

      (if all
        (repeat (sslength all)
          (setq ent (ssname all i))

          (if (member (sc:get-color ent) colors)
            (ssadd ent result)
          )

          (setq i (1+ i))
        )
      )

      ;; 5. 選択結果を反映
      (if (> (sslength result) 0)
        (progn
          (sssetfirst nil result)
          (princ
            (strcat "\n完了: "
              (itoa (sslength result))
              "個選択しました。"
            )
          )
        )
        (princ "\n該当するオブジェクトはありません。")
      )
    )
    (princ "\n基準オブジェクトが選択されていません。")
  )

  (princ)
)
