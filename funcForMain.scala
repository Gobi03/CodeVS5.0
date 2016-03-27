object funcForMain{
  private def zahyouOutput(own: PlayerState): Unit = {
    print(own.skill_sub(0))
    print(' ')
    println(own.skill_sub(1))
  }

  def output(own: PlayerState): Unit = {
    own.use_skill match{
      case Some(s) =>
        println(3)

        s match{
          case "accel" => print(0)
          case "stone_m" => print(1); print(' '); this.zahyouOutput(own)
          case "stone_o" => print(2); print(' '); this.zahyouOutput(own)
          case "thunder_m" => print(3); print(' '); this.zahyouOutput(own)
          case "thunder_o" => print(4); print(' '); this.zahyouOutput(own)
          case "avatar_m" => print(5); print(' '); this.zahyouOutput(own)
          case "avatar_o" => print(6); print(' '); this.zahyouOutput(own)
          case "cutting" => print(7);print(' ');println(own.skill_sub(0))
          case _ => println() // error
        }

        println(own.ans(0))
        println(own.ans(1))
      case None =>
        println(2)
        println(own.ans(0))
        println(own.ans(1))
    }
  }

  
}
