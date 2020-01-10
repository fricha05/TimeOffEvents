namespace TimeOff

open System


type DateProvider =
    static member DateTime =
        DateTime.Now

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | AskCancelRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | AskCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | CancelRequestAsked of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
        | RequestRefused request -> request
        | CancelRequestAsked request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | Cancelled of TimeOffRequest
        | PendingValidation of TimeOffRequest
        | Refused of TimeOffRequest
        | PendingCancellation of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Cancelled request -> request
            | PendingValidation request
            | Refused request -> request
            | PendingCancellation request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Cancelled _ -> false
            | PendingValidation _
            | Refused _ -> false
            | PendingCancellation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelled request -> Cancelled request
        | RequestRefused request -> Refused request
        | CancelRequestAsked request -> PendingCancellation request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        (request1.Start <= request2.Start && request1.End >= request2.End)
        ||
        (request2.Start <= request1.Start && request2.End >= request1.End)

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        Seq.exists (overlapsWith request) otherRequests

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= DateProvider.DateTime then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
    
    let cancelRequest requestState = 
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be cancelled"

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refused"        

    let askCancelRequest requestState = 
        match requestState with
        | Validated request ->
            Ok [CancelRequestAsked request]
        | PendingValidation request ->
            Ok [CancelRequestAsked request]
        | _ ->
            Error "You cannot ask for this request to be cancelled"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            match command with
            | CancelRequest (_, requestId) ->
                let requestState = (userRequests.Item requestId)
                if (requestState.Request.Start.Date) > DateProvider.DateTime then
                    cancelRequest requestState
                else
                    Error "Cannot cancel started or passed requests"
            | _ -> 
                Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_, requestId) ->
                let requestState = (userRequests.Item requestId)
                if (requestState.Request.Start.Date) > DateProvider.DateTime then
                    cancelRequest requestState
                else
                    Error "Cannot cancel started or passed requests"
            | RefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState
            | AskCancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                askCancelRequest requestState
                