
trait Channel {
  def id: Int
  def name: String
  def cost: Double
  def language: String
}

case class DefaultPackage(channels: List[Channel])
case class Package(name: String, channels: List[Channel])
case class Plan(name: String, duration: Int, cost: Double)

trait Subscription {
  def name: String
  def defaultPackage: DefaultPackage
  def additionalPackages: List[Package]
  def plan: Plan
}

object Subscription {
  def apply(name: String, defaultPackage: DefaultPackage, plan: Plan): Subscription =
    new Subscription {
      def name: String = name
      def defaultPackage: DefaultPackage = defaultPackage
      def additionalPackages: List[Package] = List.empty
      def plan: Plan = plan
    }

  def apply(name: String, defaultPackage: DefaultPackage, additionalPackages: List[Package], plan: Plan): Subscription =
    new Subscription {
      def name: String = name
      def defaultPackage: DefaultPackage = defaultPackage
      def additionalPackages: List[Package] = additionalPackages
      def plan: Plan = plan
    }
}

object SubscriptionService {
  def calculateCost(subscription: Subscription): Double = {
    val baseCost = subscription.defaultPackage.channels.map(_.cost).sum
    val additionalCost = subscription.additionalPackages.flatMap(_.channels).map(_.cost).sum
    baseCost + additionalCost + subscription.plan.cost
  }

  def addPackage(subscription: Subscription, packageToAdd: Package): Subscription = {
    subscription.copy(additionalPackages = subscription.additionalPackages :+ packageToAdd)
  }
}

object Main extends App {
  val channels = List(
    new Channel {
      val id: Int = 1
      val name: String = "Channel 1"
      val cost: Double = 10.0
      val language: String = "English"
    },
    new Channel {
      val id: Int = 2
      val name: String = "Channel 2"
      val cost: Double = 20.0
      val language: String = "Spanish"
    }
  )

  val defaultPackage = DefaultPackage(channels)

  val plan = Plan(name = "Monthly", duration = 30, cost = 5.0)

  val subscription = Subscription("Basic", defaultPackage, plan)

  val subscriptionWithPackage = SubscriptionService.addPackage(subscription, Package("Sports", List(channels.head)))

  val cost = SubscriptionService.calculateCost(subscriptionWithPackage)

  println(s"Total cost: $$${cost}")
}
