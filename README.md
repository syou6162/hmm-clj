# hmm-clj

Clojureで書かれた隠れマルコフモデルのライブラリ。以下の機能を持っているようなシンプルな実装のものが欲しかったので。

- 関数型らしい実装
- 単一の系列のみでなく複数系列での学習ができる
- scalingが実装してあり、長い系列でも現実的に動く
- 未知語対応

## Usage

```sh
% wget http://www.phontron.com/data/nonparametric-exercise-ja-1.tar.gz
% tar zxvf nonparametric-exercise-ja-1.tar.gz
% head -n 3000 nonparametric-exercise-ja-1/wiki-sample.example > train.txt
% tail -n 1000 nonparametric-exercise-ja-1/wiki-sample.example > test.txt
% lein test
% lein run
```

## License

Copyright © 2012 Yasuhisa Yoshida

Distributed under the Eclipse Public License, the same as Clojure.
