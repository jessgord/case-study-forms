package forms

final case class Form[A](title: String, question: Question[A])
