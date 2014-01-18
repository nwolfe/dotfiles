require 'rake'

SKIP = %w[Rakefile README.md osx puppetrc]

task :install do Dir['*'].each do |file|
    next if SKIP.include? file

    if File.exist? File.join(ENV['HOME'], ".#{file}")
      maybe_replace file
    else
      install file
    end
  end
end

def install(file)
  puts "Linking ~/.#{file}"
  source = File.join(ENV['PWD'], file)
  target = File.join(ENV['HOME'], '.' + file)
  `ln -s "#{source}" "#{target}"`
end

def maybe_replace(file)
  print "overwrite ~/.#{file}? [yn] "

  case $stdin.gets.chomp 
  when 'y'
    `rm "#{File.join(ENV['HOME'], '.' + file)}"`
    install file
  else 
    puts "skipping ~/.#{file}"
  end
end
