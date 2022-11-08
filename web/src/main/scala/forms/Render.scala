package forms

import com.raquo.laminar.api.L._
import org.scalajs.dom.window.alert

object Render {

def render[A](form: Form[A]): HtmlElement = {
  val (html, signal) = render(form.question)
  val submitted = new EventBus[Unit]

    div(
      className := "p-4",
      h1(className := "text-4xl font-bold mb-4", form.title),
      html,
      button(
        className := "rounded-md border-solid border-2 border-sky-500 m-2 p-2", 
        typ := "submit", "Submit", 
        onClick.mapTo(()) --> submitted.writer
      ),
      submitted.events.sample(signal) --> ((msg: A) => alert(s"The meaning of life is $msg"))
    )
}

  def render[A](question: Question[A]): (HtmlElement, Signal[A]) =
      question match {
        case TextQuestion(initialValue, textLabel, pHolder) =>
          val textValue = Var("")
          val html = 
            div(
              renderLabel(textLabel),
              input(
                className := "ring-2 ring-sky-500 border-2 border-solid border-sky-500 ml-2 p-1",
                typ := "text",
                initialValue.map(iV => value := iV).orElse(pHolder.map(pH => placeholder := pH)),
                onInput.mapToValue --> textValue
              )
              //  p(
              //   "You typed: ",
              //   child.text <-- textValue
              //   )
            )
          (html, textValue.signal)
          
        case IntQuestion(initialValue, intLabel, pHolder, predicate) =>
          val intValue = Var(0)
          val html =
            div(
              renderLabel(intLabel),
              input(
                typ := "number",
                initialValue.map(iV => value := iV.toString).orElse(pHolder.map(pH => placeholder := pH.toString)),
                onInput.mapToValue.map(str => str.toInt) --> intValue
              ),
              p(State.validate(intValue, predicate, "ERROR TBC"))
              // p(
              //   "Your lucky number is: ",
              //   child.text <-- intValue
              // )
            )
          (html, intValue.signal)

        case BooleanQuestion(style @ _, initialValue @ _, boolLabel) =>
          import Style.BooleanStyle._
          val boolValue = Var(false)
          val html =
            style match {
              case Checkbox =>
                div(
                  renderLabel(boolLabel),
                  input(
                    typ := "checkbox",
                    name := "true",
                    onInput.mapToChecked --> boolValue
                  ),
                  p(
                  s"$boolLabel",
                  child.text <-- boolValue
                  )
                )
              case Choice(t, f) => 
                div(
                  renderLabel(boolLabel),
                  renderLabel(t), input(typ := "radio", onInput.mapToChecked --> boolValue),
                  renderLabel(f), input(typ := "radio", onInput.mapToChecked --> boolValue),
                  p(
                  s"$boolLabel",
                  child.text <-- boolValue
                  )
                )
                
            }
            (html, boolValue.signal)

        case MultipleChoice(multipleLabel, choices) =>
          val multiList = Var(List[A]())
          val html =
            div(
              renderLabel(multipleLabel),
              choices.map { case (prompt, value @ _) =>
                div(
                  input(typ := "checkbox", 
                        name := prompt),
                        // onInput.sample(multiList).map(lst => value :: lst) --> multiList),
                  label(prompt)
                )
              }
              // p(
              //   "You choose: ",
              //   child.text <-- multiList.signal.map(lst => lst.mkString(","))
              // )
            )
          (html, multiList.signal)

        case Product(left, right) =>
          val (leftHtml, leftSignal) = render(left)
          val (rightHtml, rightSignal) = render(right)
          val html = div(leftHtml, rightHtml)
          val signal = leftSignal.combineWith(rightSignal)
          (html, signal)
      }
    

    def renderLabel(lbl: Option[String]): HtmlElement =
      label(className := "mr-2", lbl)

    def renderLabel(lbl: String): HtmlElement =
      label(className := "mr-2", lbl)
}
