#! /usr/bin/env ruby

require 'date'

file_name = ARGV[0]

def get_date(line)
  parser = /^(?<remote>[^ ]*) (?<host>[^ ]*) (?<user>[^ ]*) \[(?<time>[^\]]*)\] "(?<method>\S+)(?: +(?<path>[^\"]*) +\S*)?" (?<code>[^ ]*) (?<size>[^ ]*)(?: "(?<referer>[^\"]*)" "(?<agent>[^\"]*)")?/
  match = line.match(parser)
  DateTime.strptime(match[:time], "%d/%b/%Y:%H:%M:%S %Z")
end

# start_time = DateTime.new(3000,1,1,0,0,0)
# end_time = Time.at(0).utc.to_datetime

# File.readlines(file_name).each do |line|
#   match = line.match(parser)

#   time = DateTime.strptime(match[:time], "%d/%b/%Y:%H:%M:%S %Z")

#   if time < start_time then
#     start_time = time
#   end

#   if time > end_time then
#     end_time = time
#   end
# end

start_time = get_date(`head -n 1 #{file_name}`)
end_time = get_date(`tail -n 1 #{file_name}`)

puts "#{start_time} => #{end_time}"
