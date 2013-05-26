require 'rake'

task :install do
  Dir['*'].each do |file|
    next if file == "Rakefile"

    if File.exist? File.join(ENV['HOME'], ".#{file}")
      maybe_replace file
    else
      install file
    end
  end
end

def install(file)
  #puts "Linking ~/.#{file}"
  system %Q{ln -s "$PWD/#{file}" "$HOME/.#{file}"}
end

def maybe_replace(file)
  print "overwrite ~/.#{file}? [yn] "

  case $stdin.gets.chomp 
  when 'y'
    system %Q{rm "$HOME/.#{file}"}
    install(file)
  else 
    puts "skipping ~/.#{file}"
  end
end
