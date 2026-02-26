{ name ? "pelotero-engine", ... }:
{
  inherit name;

  network = {
    host = "localhost";
    bindAddress = "0.0.0.0";
  };

  database = {
    name = "fantasy_league";
    user = "$(whoami)";
    password = "postgres";
    port = 5432;
    dataDir = "$HOME/.local/share/${name}/postgres";
    settings = {
      max_connections = 100;
      shared_buffers = "128MB";
      dynamic_shared_memory_type = "posix";
      log_destination = "stderr";
      logging_collector = true;
      log_directory = "log";
      log_filename = "postgresql-%Y-%m-%d_%H%M%S.log";
      log_min_messages = "info";
      log_min_error_statement = "info";
      log_connections = true;
      listen_addresses = "localhost";
    };
  };

  haskell = {
    port = 8080;
    cabalFile = "./pelotero-engine.cabal";
    codeDirs = [
      "./src"
      "./src-new"
      "./app"
    ];
    tests = "./test";
  };

  dataDir = "$HOME/.local/share/${name}";
}