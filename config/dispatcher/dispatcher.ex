defmodule Dispatcher do
  use Matcher
  define_accept_types []

  # def start(_argv) do
  #   port = 80
  #   IO.puts "Starting Plug with Cowboy on port #{port}"
  #   Plug.Adapters.Cowboy.http __MODULE__, [], port: port
  #   :timer.sleep(:infinity)
  # end

  # plug Plug.Logger
  # plug :match
  # plug :dispatch

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end

  match "/export/orders/*path" do
    Proxy.forward conn, path, "http://export-orders/"
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
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/favourites/*path" do
    Proxy.forward conn, path, "http://resource/favourites/" 
  end

  match "/sessions/*path" do
    Proxy.forward conn, path, "http://authentication/sessions/" 
  end

  match "/organizations/*path" do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/delivery-places/*path" do
    Proxy.forward conn, path, "http://cache/delivery-places/"
  end

  match "/delivery-kinds/*path" do
    Proxy.forward conn, path, "http://cache/delivery-kinds/"
  end

  match "/geo-coordinates/*path" do
    Proxy.forward conn, path, "http://cache/geo-coordinates/"
  end

  match "/postal-addresses/*path" do
    Proxy.forward conn, path, "http://cache/postal-addresses/"
  end

  match "/product-groups/*path" do
    Proxy.forward conn, path, "http://cache/product-groups/"
  end

  match "/products/*path" do
    Proxy.forward conn, path, "http://cache/products/"
  end

  match "/spotlight-products/*path" do
    Proxy.forward conn, path, "http://cache/spotlight-products/"
  end

  match "/counttriples/*path" do
    Proxy.forward conn, path, "http://counttriplesservice/count/"
  end

  match "/offerings/*path" do
    Proxy.forward conn, path, "http://cache/offerings/"
  end

  match "/banners/*path" do
    Proxy.forward conn, path, "http://cache/banners/"
  end

  match "/unit-price-specifications/*path" do
    Proxy.forward conn, path, "http://cache/unit-price-specifications/"
  end

  match "/quantitative-values/*path" do
    Proxy.forward conn, path, "http://cache/quantitative-values/"
  end

  match "/type-and-quantities/*path" do
    Proxy.forward conn, path, "http://cache/type-and-quantities/"
  end

  match "/baskets/*path" do
    Proxy.forward conn, path, "http://cache/baskets/"
  end

  match "/order-lines/*path" do
    Proxy.forward conn, path, "http://cache/order-lines/"
  end

  match "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/current-basket/*path" do
    Proxy.forward conn, path, "http://basketservice/"
  end

  match "/confirm-basket/*path" do
    Proxy.forward conn, path, "http://basketservice/confirm/"
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

  match "/full-addresses/*path" do
    Proxy.forward conn, path, "http://cache/full-addresses/"
  end

  match "/*_", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex." )
  end

end
