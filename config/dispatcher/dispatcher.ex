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

  match "/export/*path" do
    Proxy.forward conn, path, "http://export-service/" 
  end

  match "/people/*path" do
    Proxy.forward conn, path, "http://resource/people/" 
  end

  post "/accounts/*path" do
    Proxy.forward conn, path, "http://authentication/accounts/" 
  end

  delete "/accounts/current/*path" do
    Proxy.forward conn, path, "http://authentication/accounts/current/" 
  end

  patch "/accounts/current/changePassword/*path" do
    Proxy.forward conn, path, "http://authentication/accounts/current/changePassword/" 
  end

  get "/accounts/*path" do
    Proxy.forward conn, path, "http://resource/accounts/" 
  end

  match "/favourites/*path" do
    Proxy.forward conn, path, "http://resource/favourites/" 
  end

  match "/sessions/*path" do
    Proxy.forward conn, path, "http://authentication/sessions/" 
  end

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

  match "/product-groups/*path" do
    Proxy.forward conn, path, "http://resource/product-groups/"
  end

  match "/products/*path" do
    Proxy.forward conn, path, "http://resource/products/"
  end

  match "/spotlight-products/*path" do
    Proxy.forward conn, path, "http://resource/spotlight-products/"
  end

  match "/counttriples/*path" do
    Proxy.forward conn, path, "http://counttriplesservice/count/"
  end

  match "/offerings/*path" do
    Proxy.forward conn, path, "http://resource/offerings/"
  end

  match "/unit-price-specifications/*path" do
    Proxy.forward conn, path, "http://resource/unit-price-specifications/"
  end

  match "/quantitative-values/*path" do
    Proxy.forward conn, path, "http://resource/quantitative-values/"
  end

  match "/type-and-quantities/*path" do
    Proxy.forward conn, path, "http://resource/type-and-quantities/"
  end

  match "/baskets/*path" do
    Proxy.forward conn, path, "http://resource/baskets/"
  end

  match "/order-lines/*path" do
    Proxy.forward conn, path, "http://resource/order-lines/"
  end

  match "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/current-basket/*path" do
    Proxy.forward conn, path, "http://basketservice/"
  end


  match "/payments/*path" do
    Proxy.forward conn, path, "http://payments/payments/"
  end

  match "/paymentWebhook/*path" do
    Proxy.forward conn, path, "http://payments/webhook/"
  end

  match "/images/*path" do
    Proxy.forward conn, path, "http://imageservice/image/"
  end


  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex." )
  end

end
