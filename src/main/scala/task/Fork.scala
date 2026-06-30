package task
//Одна вилка, у которой есть id и условие занята она или нет 
final case class Fork(id: Int, takeBy: Option[Int])