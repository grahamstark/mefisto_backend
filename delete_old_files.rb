# delete_old_files.rb
#
# This Ruby script deletes all directories starting
# tmp_user_* and a day or more older from
# c:/euromod/mefisto_backend/ada/working/users .
	
require 'FileUtils'

one_day = 24 * 60 * 60

Dir.chdir("c:/euromod/mefisto_backend/ada/working/users")

#puts Dir.getwd

files = Dir.glob("tmp_user_*")

#puts files

files.each { |file|

  #puts
  #puts file

  mod_time = File.mtime( file )
  #puts mod_time.strftime("%A %B %d %I:%M %p" )

  time_now = Time.now
  #puts time_now.strftime("%A %B %d %I:%M %p" )

  age = time_now - mod_time
  #puts age

  if ( age >= one_day ) 
    #puts "File is one day or older."
    FileUtils.rm_rf file
    #puts "Deleted."
  else
    #puts "File is too new."
  end

}
