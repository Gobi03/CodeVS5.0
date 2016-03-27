_ : floor
W : wall
O : 岩
D : 犬
S : 忍者ソウル
N : 忍者
o : 岩 & 忍者ソウル
d : 犬 & 忍者ソウル

def copy_chizu(map: Array[String]): Array[Array[Char]] = {
  val res = new Array[Array[Char]](17)
  for(i <- 0 until 17)
    res(i) = map(i).toArray
  return res
}

// cnt: 現在距離, range: 索敵距離
def DFS(cnt: Int, range: Int, x: Int, y: Int): Unit = {
  // 指定距離索敵するか、x, yが壁に入るとき打ち切り
  if(cnt == range || x <= 0 || x >= 13 || y <= 0 || y >= 16)
    return
  else if(chizu(y)(x) == )
}

/// BFS
val q = new scala.collection.mutable.Queue[Int]
q += 初期値

while(!q.isEmpty){
  val now = q.dequeue

  // 状態 now に対する処理

  q += next
}



/// debug functions
// output chizu
def outputChizu(map: Array[Array[Char]]): Unit = {
  for(h <- 0 until 17){
    for(w <- 0 until 14)
      print(map(h)(w))
    println()
  }
}
