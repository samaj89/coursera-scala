package observer

trait Subscriber {
  def handler(pub: Publisher)
}
