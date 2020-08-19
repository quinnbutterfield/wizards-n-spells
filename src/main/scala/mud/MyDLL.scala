package mud

class MyDLL[A] {
  import MyDLL.Node
  private var default: A = _
  
  private var end: Node[A] = new Node[A](null, default, null)
  end.prev = end
  end.next = end
  private var _length = 0
  
  def apply(index: Int): A = {
    require(index >= 0 && index < _length)
    var rover = end.next
    for(_ <- 0 until index) rover = rover.next
    rover.data
  }
  
  def update(index: Int, a:A):Unit = {
    require(index >= 0 && index < _length)
    var rover = end.next
    for (_ <- 0 until index) rover = rover.next
    rover.data = a
  }
   def add(index: Int, a: A): Unit = {
    require(index >= 0 && index <= _length)
    var rover = end
    for (_ <- 0 until index) rover = rover.next
    val n = new Node[A](rover, a, rover.next)
    rover.next.prev = n
    rover.next = n
    _length += 1
  }

  def remove(index: Int): A = {
    require(index >= 0 && index < _length)
    _length -= 1
    var rover = end.next
    for (_ <- 0 until index) rover = rover.next
    rover.prev.next = rover.next
    rover.next.prev = rover.prev
    rover.data
  }

  def length: Int = _length
  
  def +=(a: A): MyDLL[A] = {
    val n = new Node[A](end.prev, a, end)
    end.prev.next = n
    end.prev = n
    _length += 1
    this
  }
  
  def filter(pred: A => Boolean): MyDLL[A] = {
    val ret = new MyDLL[A]()
    var rover = end.next
    while(rover != end) {
      if(pred(rover.data)) {
        ret += rover.data
      }
      rover = rover.next
    }
    ret
  }
 /* def contains(a:A):Boolean = {
    var rover = end.next
    
  }*/
}
object MyDLL {
  private class Node[A](var prev: Node[A], var data: A, var next: Node[A])
}














