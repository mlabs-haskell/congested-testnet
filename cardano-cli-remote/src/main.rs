/* actix-web service to connect post request with cardano-cli */
// curl -X POST http://congested-testnet.staging.mlabs.city:8001/cli \
//      -H "Content-Type: application/json" \
//      -d '{"args": "argument1 argument2 argument3"}'

use actix_web::{web, App, HttpServer, Responder, HttpResponse};
use serde::Deserialize;
use std::process::Command;

#[derive(Deserialize)]
struct Args {
    args: String,
}

async fn run_cardano_cli(args: web::Json<Args>) -> impl Responder {
    let args_vec: Vec<&str> = args.args.split_whitespace().collect();
    let output = Command::new("cardano-cli")
        .args(&args_vec)
        .output()
        .expect("Failed to execute command");

    if output.status.success() {
        HttpResponse::Ok().content_type("text/plain").body(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        HttpResponse::InternalServerError().content_type("text/plain").body(String::from_utf8_lossy(&output.stderr).into_owned())
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/cli", web::post().to(run_cardano_cli))
    })
    .bind("0.0.0.0:8001")?
    .run()
    .await
}
