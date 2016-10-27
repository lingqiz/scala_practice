package objsets

import common._
import TweetReader._


class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

abstract class RetweetSet
{
	def incl(elem: Tweet): RetweetSet
	def toList(list: TweetList): TweetList
}

class NonEmptyRe(elem: Tweet, left: RetweetSet, right: RetweetSet) extends RetweetSet
{
	def incl(x: Tweet): RetweetSet = 
	  if	  (x.retweets > elem.retweets) new NonEmptyRe(elem, left, right incl x)
	  else if (x.retweets < elem.retweets) new NonEmptyRe(elem, left incl x, right)
	  else this
	  
	def toList(list: TweetList): TweetList = 
	  right.toList(new Cons(elem, left.toList(list)))
}

object EmptyRe extends RetweetSet
{
	def incl(x: Tweet) = new NonEmptyRe(x, EmptyRe, EmptyRe)
	def toList(list: TweetList) = list
}


abstract class TweetSet 
{
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
  
  def union(that: TweetSet): TweetSet
  
  def mostRetweeted: Tweet
  
  def descendingByRetweet: TweetList
  
  def incl(tweet: Tweet): TweetSet
  
  def remove(tweet: Tweet): TweetSet
  
  def contains(tweet: Tweet): Boolean
  
  def foreach(f: Tweet => Unit): Unit
  
  def isEmpty: Boolean
  
  def toRetSet(set: RetweetSet): RetweetSet
}

class Empty extends TweetSet 
{
  def mostRetweeted = throw new java.util.NoSuchElementException
  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
		  
  def descendingByRetweet = Nil
  
  /**
   * The following methods are already implemented
   */
  def union(that: TweetSet) = that

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  def isEmpty = true
  
  def toRetSet(set: RetweetSet) = set
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  
  def descendingByRetweet: TweetList = toRetSet(EmptyRe).toList(Nil)
  
  def toRetSet(set: RetweetSet) = 
    right toRetSet (left toRetSet (set incl elem))

  def mostRetweeted = retweetedHelper((x, y) => x > y, Int.MinValue)
  
  def retweetedHelper(c: (Int, Int) => Boolean, v: Int): Tweet = 
  {
    def lf = if (left.isEmpty) null else left .mostRetweeted
    def rg = if(right.isEmpty) null else right.mostRetweeted
    
    def count(t: Tweet) = if(t == null) v else t.retweets
    val mid = if(c(count(elem), count(lf))) elem else lf
    
    if(c(count(mid), count(rg))) mid else rg
    
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =  
	right.filterAcc(p, left.filterAcc(p, if(p(elem)) acc incl elem else acc))
  
  def union(that: TweetSet) =  left union (right union (that incl elem))
  
  def isEmpty = false
  
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if 		(tw.text < elem.text) 	new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) 	new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def iterTect(keys: List[String], value: Boolean)(elem: Tweet): Boolean =
    if(keys.isEmpty) value
    else iterTect(keys.tail, value || elem.text.contains(keys.head))(elem)
  
  lazy val googleTweets: TweetSet = 
    TweetReader.allTweets.filter(iterTect(google, false))
  
  lazy val appleTweets: TweetSet = 
    TweetReader.allTweets.filter(iterTect(apple, false))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (appleTweets union googleTweets).descendingByRetweet 
}

object Main extends App 
{
  // Print the trending tweets
  GoogleVsApple.trending foreach println

}
