module Cranson exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, andThen, field, int, nullable, string, succeed)
import Json.Decode.Extra exposing ((|:))


-- MODEL


type alias Model =
    { plans : List Plan
    , viewState : ViewState
    , alertMessage : Maybe String
    }


type ViewState
    = ViewAll
    | ViewSingle


type Status
    = Pending
    | Decided
    | OnAppeal
    | InvalidOrWithdrawn
    | InProcess
    | Unknown


type alias Plan =
    { id : Int
    , address : String
    , description : String
    , reference : String
    , status : Status
    , registrationDate : String
    , decisionDate : Maybe String
    , link : String
    }


initialModel : Model
initialModel =
    { plans = []
    , viewState = ViewAll
    , alertMessage = Nothing
    }



-- DECODERS/ENCODERS


planDecoder : Decoder Plan
planDecoder =
    succeed Plan
        |: field "id" int
        |: field "address" string
        |: field "description" string
        |: field "reference" string
        |: field "status" (string |> andThen decodeStatus)
        |: field "registration_date" string
        |: field "decision_date" (nullable string)
        |: field "link" string


decodeStatus : String -> Decoder Status
decodeStatus status =
    succeed (planStatus status)


planStatus : String -> Status
planStatus status =
    case status of
        "Pending" ->
            Pending

        "Decided" ->
            Decided

        "Invalid or Withdrawn" ->
            InvalidOrWithdrawn

        "On Appeal" ->
            OnAppeal

        "Current status not assigned in APAS" ->
            InProcess

        _ ->
            Unknown



-- COMMANDS


baseUrl : String
baseUrl =
    "http://api.cranson.co"


plansUrl : String
plansUrl =
    baseUrl ++ "/plans"


planUrl : Plan -> String
planUrl plan =
    baseUrl ++ "/plan/" ++ toString plan.id


getPlans : Cmd Msg
getPlans =
    Json.Decode.list planDecoder
        |> Http.get plansUrl
        |> Http.send NewPlans



-- UPDATE


type Msg
    = NewPlans (Result Http.Error (List Plan))
    | CloseAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPlans (Ok plans) ->
            ( { model | plans = plans }, Cmd.none )

        NewPlans (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            toString response.status

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            toString error



-- VIEW


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [ class "text-center" ] [ text title ] ]


viewPlansList : List Plan -> Html msg
viewPlansList plans =
    let
        listOfPlans =
            List.map viewPlanItem plans
    in
    div [ class "plans" ]
        [ div [ class "row" ]
            [ div [ class "col-12" ] [ h3 [ class "text-center" ] [ text "Latest planning applications" ] ]
            ]
        , div [ class "row" ]
            listOfPlans
        ]


viewPlanItem : Plan -> Html msg
viewPlanItem plan =
    div [ class "col-3" ]
        [ div [ class "card mb-2 plan" ]
            [ div [ class "card-header" ]
                [ h5 [ class "card-title" ] [ text plan.address ]
                ]
            , div [ class "card-body" ]
                [ p [ class "card-text" ] [ text plan.description ]
                , p [ class "card-text" ]
                    [ small [ class "text-muted" ] [ text ("Registered: " ++ plan.registrationDate) ]
                    ]
                ]
            , div [ class "card-footer" ]
                [ div [ class "row" ]
                    [ div []
                        [ span [ class "badge badge-secondary" ] [ text (toString plan.status) ] ]
                    ]
                ]
            ]
        ]


view : Model -> Html msg
view model =
    div [ class "content" ]
        [ viewHeader "Cranson"
        , viewPlansList model.plans
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getPlans )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
