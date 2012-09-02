# ruby

load 'flussonic.rb'

# use Rack::Auth::Basic, "Lobster 2.0" do |username, password|
#   'secret' == password
# end

run Flussonic.new
