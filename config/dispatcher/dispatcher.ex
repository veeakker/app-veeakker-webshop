defmodule Dispatcher do
  use Plug.Router

  def start(_argv) do
    port = 80
    IO.puts "Starting Plug with Cowboy on port #{port}"
    Plug.Adapters.Cowboy.http __MODULE__, [], port: port
    :timer.sleep(:infinity)
  end

  plug Plug.Logger
  plug :match
  plug :dispatch

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end

  match "/organizations/*path" do
    Proxy.forward conn, path, "http://resource/organizations/"
  end

  match "/delivery-places/*path" do
    Proxy.forward conn, path, "http://resource/delivery-places/"
  end

  match "/delivery-kinds/*path" do
    Proxy.forward conn, path, "http://resource/delivery-kinds/"
  end

  match "/geo-coordinates/*path" do
    Proxy.forward conn, path, "http://resource/geo-coordinates/"
  end

  match "/postal-addresses/*path" do
    Proxy.forward conn, path, "http://resource/postal-addresses/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
