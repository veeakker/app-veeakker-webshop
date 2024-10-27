defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    image: ["image/*"],
    any: [ "*/*" ]
  ]

  define_layers [ :static, :frontend_html, :api, :frontend_fallback, :not_found ]

  @json_api %{ accept: [:json], layer: :api  }
  @image %{ accept: [:image] }

  match "/.well-known/*_path", _ do
    send_resp( conn, 200, "{ \"message\": \"ok\" }" )
  end

  match "/favicon.ico/*_path", _ do
    send_resp( conn, 404, "No icon specified" )
  end

  match "/assets/*path", %{ layer: :static, reverse_host: ["admin" | _rest ] } do
    Proxy.forward conn, path, "http://frontendfreddie/assets/"
  end

  match "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontendwebshop/assets/"
  end

  match "/images/*path", %{ layer: :static, reverse_host: ["admin" | _rest ] } do
    Proxy.forward conn, path, "http://frontendfreddie/images/"
  end

  match "/images/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontendwebshop/images/"
  end

  get "/search/:type/*path", @json_api do
    new_path = "http://search/#{type}/search/"
    IO.puts "Posting to #{new_path}"
    Proxy.forward conn, path, new_path
  end

  match "/people/*path", @json_api do
    Proxy.forward conn, path, "http://authentication/people/"
  end

  match "/postal-addresses/*path", @json_api do
    Proxy.forward conn, path, "http://cache/postal-addresses/"
  end

  patch "/accounts/*path", @json_api do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  post "/accounts/*path", @json_api do
    Proxy.forward conn, path, "http://authentication/accounts/"
  end

  delete "/accounts/current/*path", @json_api do
    Proxy.forward conn, path, "http://authentication/accounts/current/"
  end

  patch "/accounts/current/changePassword/*path", @json_api do
    Proxy.forward conn, path, "http://authentication/accounts/current/changePassword/" 
  end

  get "/accounts/*path", @json_api do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/favourites/*path", @json_api do
    Proxy.forward conn, path, "http://resource/favourites/"
  end

  match "/sessions/*path", @json_api do
    Proxy.forward conn, path, "http://authentication/sessions/"
  end

  match "/organizations/*path", @json_api do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/delivery-places/*path", @json_api do
    Proxy.forward conn, path, "http://cache/delivery-places/"
  end

  match "/delivery-routes/*path", @json_api do
    IO.puts "Yay delivery routes"
    Proxy.forward conn, path, "http://cache/delivery-routes/"
  end

  match "/delivery-kinds/*path", @json_api do
    Proxy.forward conn, path, "http://cache/delivery-kinds/"
  end

  match "/geo-coordinates/*path", @json_api do
    Proxy.forward conn, path, "http://cache/geo-coordinates/"
  end

  match "/product-groups/*path", @json_api do
    Proxy.forward conn, path, "http://cache/product-groups/"
  end

  match "/products/*path", @json_api do
    Proxy.forward conn, path, "http://cache/products/"
  end

  match "/spotlight-products/*path", @json_api do
    Proxy.forward conn, path, "http://cache/spotlight-products/"
  end

  match "/counttriples/*path", @json_api do
    Proxy.forward conn, path, "http://counttriplesservice/count/"
  end

  match "/offerings/*path", @json_api do
    Proxy.forward conn, path, "http://cache/offerings/"
  end

  match "/banners/*path", @json_api do
    Proxy.forward conn, path, "http://cache/banners/"
  end

  match "/unit-price-specifications/*path", @json_api do
    Proxy.forward conn, path, "http://cache/unit-price-specifications/"
  end

  match "/quantitative-values/*path", @json_api do
    Proxy.forward conn, path, "http://cache/quantitative-values/"
  end

  match "/type-and-quantities/*path", @json_api do
    Proxy.forward conn, path, "http://cache/type-and-quantities/"
  end

  match "/baskets/*path", @json_api do
    Proxy.forward conn, path, "http://resource/baskets/"
  end

  match "/order-lines/*path", @json_api do
    Proxy.forward conn, path, "http://cache/order-lines/"
  end

  match "/files/*path", @json_api do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/current-basket/*path", @json_api do
    Proxy.forward conn, path, "http://basketservice/"
  end

  match "/confirm-basket/*path", @json_api do
    Proxy.forward conn, path, "http://basketservice/confirm/"
  end

  match "/imgs/*path", %{ layer: :static, accept: [:image] } do
    Proxy.forward conn, path, "http://imageservice/image/"
  end

  match "/full-addresses/*path", @json_api do
    Proxy.forward conn, path, "http://cache/full-addresses/"
  end

  match "/export/changed/*path", @json_api do
    Proxy.forward conn, path, "http://export-orders/changed/"
  end

  match "/export/baskets/*path", @json_api do
    Proxy.forward conn, path, "http://export-orders/baskets/"
  end

  match "/*_", %{ accept: [:html], layer: :frontend_html, reverse_host: ["admin" | _rest ] } do
    Proxy.forward conn, [], "http://frontendfreddie/index.html"
  end

  match "/*_", %{ accept: [:html], layer: :frontend_html } do
    Proxy.forward conn, [], "http://frontendwebshop/index.html"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex." )
  end

end
