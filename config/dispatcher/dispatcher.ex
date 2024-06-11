defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ]
  ]

  @json %{ accept: %{ json: true } }

  # temporarily disabled
  # match "/export/orders/*path" do
  #   Proxy.forward conn, path, "http://export-orders/"
  # end

  get "/search/:type/*path", @json do
    new_path = "http://search/#{type}/search/"
    IO.puts "Posting to #{new_path}"
    Proxy.forward conn, path, new_path
  end

  match "/people/*path" do
    Proxy.forward conn, path, "http://authentication/people/"
  end

  # match "/postal-addresses/*path" do
  #   Proxy.forward conn, path, "http://authentication/postal-addresses/"
  # end
  match "/postal-addresses/*path" do
    Proxy.forward conn, path, "http://cache/postal-addresses/"
  end

  patch "/accounts/*path" do
    Proxy.forward conn, path, "http://cache/accounts/"
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
    Proxy.forward conn, path, "http://resource/baskets/"
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

  # match "/payments/*path" do
  #   Proxy.forward conn, path, "http://payments/payments/"
  # end

  # match "/paymentWebhook/*path" do
  #   Proxy.forward conn, path, "http://payments/webhook/"
  # end

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
