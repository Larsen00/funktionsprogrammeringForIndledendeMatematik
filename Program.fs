module Program

// open Microsoft.AspNetCore.Hosting
// open Microsoft.Extensions.Hosting
// open Giraffe

// let webApp =
//     choose [
//         route "/processInput/{input}" >=> Api.processInputHandler
//         setStatusCode 404 >=> text "Not found"
//     ]

// let configureApp (app: IApplicationBuilder) =
//     app.UseRouting().UseEndpoints(fun endpoints -> endpoints.MapGiraffe webApp)

// let configureServices (services: IServiceCollection) =
//     services.AddRouting()

[<EntryPoint>]
let main args =
    // WebHost.CreateDefaultBuilder(args)
    //     .Configure(configureApp)
    //     .ConfigureServices(configureServices)
    //     .Build().Run()
    0
