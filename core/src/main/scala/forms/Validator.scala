package forms

sealed trait State
final case class Valid[A](value: A) extends State
final case class Invalid(error: String) extends State

// 1. a function which takes the input and applies a conditional to it and returns Valid or Invalid
// (e.g. if an integer is less than 10)

// 2. Pattern match based on results of 1.. if Valid then process the form if Invalid display error message on screen
object State {
    def validate[A](input: A, predicate: Option[A => Boolean], errorMessage: String): State = {
        predicate match {
            case Some(p) =>
                 if (p(input)) Valid[A](input)
                 else Invalid(errorMessage)
            case None => Valid(input)
        }
    }
}
