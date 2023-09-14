package funscala.ch6

//type State[S,+A] = S ⇒ (A,S)

/** Book's example */
case class State[S,+A](run: S ⇒ (A,S))

object State {

}
