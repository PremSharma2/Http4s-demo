import cats._
import cats.effect._
import cats.implicits._
import org.http4s.circe._
import org.http4s._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.dsl._
import org.http4s.dsl.impl._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.server._
import org.http4s.server.blaze.BlazeServerBuilder

import java.time.Year
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

object Http4sTutorial1 extends IOApp{


  //movie database
  type Actor = String

  case class Movie(id: String, tittle: String, year: Int, actors: List[Actor], director: String)
   case class DirectorDetails(firstName:String,lastName:String,genre:String)
  case class Director(firstName: String, lastName: String) {
    override def toString: Actor = s"$firstName  $lastName"
  }
  val snjl: Movie = Movie(
    "6bcbca1e-efd3-411d-9f7c-14b872444fce",
    "Zack Snyder's Justice League",
    2021,
    List("Henry Cavill", "Gal Godot", "Ezra Miller", "Ben Affleck", "Ray Fisher", "Jason Momoa"),
    "Zack Snyder"
  )

  val movies: Map[String, Movie] = Map(snjl.id -> snjl)

  private def findMovieById(movieId: UUID) =
    movies.get(movieId.toString)

  private def findMoviesByDirector(director: String): List[Movie] =
    movies.values.filter(_.director == director).toList

  /*
Lets Define end point now
Get ALL movies for a Director under a given year
Get ALL Actors for a Movie
Post End point to Add a new Director to platform

Http Server in in FP is defined as function
Request -> F[Option[Response]]
F is mostly IO here
HttpRoutes[F] denotes the above function in scala Http4s API

Route Defination:
We can imagine the route that returns
the list of movies of a director
as something similar to the following:

GET /movies?director=Zack%20Snyder&year=2021
As we said, every route corresponds to an instance of the HttpRoutes[F] type.
Again, the http4s library helps us define such routes,
providing us with a dedicated DSL, the http4s-dsl.

Through the DSL, we build an HttpRoutes[F]
using pattern matching as a sequence of case statements.
 So, let’s do it:
 */
  object DirectorQueryParamMatcher extends QueryParamDecoderMatcher[String]("director")

  object YearQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Year]("year")

  object YearQueryParamMatchers extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")

  implicit val yearQueryParamDecoder1: QueryParamDecoder[Year] =
    QueryParamDecoder[Int].emap { y =>
      Try(Year.of(y))
        .toEither
        .leftMap { tr =>
          ParseFailure(tr.getMessage, tr.getMessage)
        }
    }

  //implicit val yearQueryParamDecoder: QueryParamDecoder[Year] =
  //  QueryParamDecoder[Int].map(Year.of)

  def movieRoutes[F[_] : Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "movies" :? DirectorQueryParamMatcher(director) +& YearQueryParamMatchers(maybeYear) =>
        val moviesBydirector= findMoviesByDirector(director)
        maybeYear match {
          case Some(validated) =>
            validated.fold(
              _ => BadRequest("The given year is not valid"),
              year =>{

                val moviesBydirectorandyear=moviesBydirector.filter(_.year== year.getValue)
                // Proceeding with the business logic
                Ok(moviesBydirectorandyear.asJson)
              }

            )
          case None => Ok(moviesBydirector.asJson)
        }

      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
        findMovieById(movieId).map(_.actors) match {
          case Some(actors) => Ok(actors.asJson)
          case _ =>         NotFound(s"No Movie for this movieId $movieId In the Database!!")
        }
    }

  }
    object DirectorVar {
      def unapply(str: String): Option[Director] = {
        if (str.nonEmpty && str.matches(".* .*")) {
          Try {
            val splitStr = str.split(' ')
            Director(splitStr(0), splitStr(1))
          }.toOption
        } else None
      }
    }

//DB
  val directorDtailsDB:mutable.Map[Director,DirectorDetails]= {
  mutable.Map(Director("Zack","Snyder") -> DirectorDetails("Zack","Snyder","superhero"))
  }

    //Directors End Point
    def directorRoutes[F[_] : Monad]: HttpRoutes[F] = {
      val dsl = Http4sDsl[F]
      import dsl._
      HttpRoutes.of[F] {
        case GET -> Root / "directors" / DirectorVar(director) =>
          directorDtailsDB.get(director) match {
            case Some(dirDetails) => Ok(dirDetails.asJson)
            case _ => NotFound(s"No director $director found in DB ")
          }
      }
    }
//Kleisli[F, Request[G], Response[G]] == HttpRoutes[F]
    def allRoutes[F[_] : Monad]: HttpRoutes[F] = {
      import cats.syntax.semigroupk._
      movieRoutes[F] <+> directorRoutes[F]
    }

  def allRoutesComplete[F[_] : Monad]: HttpApp[F] = {
    allRoutes[F].orNotFound
  }


  import scala.concurrent.ExecutionContext.global

  override def run(args: List[String]): IO[ExitCode] = {

    val apis = Router(
      "/api" -> Http4sTutorial.movieRoutes[IO],
      "/api/private" -> Http4sTutorial.directorRoutes[IO]
    ).orNotFound

    BlazeServerBuilder[IO](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(apis)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
  }




