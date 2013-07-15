require 'rubygems'
require "rexml/document";
require 'sinatra';
require 'liquid';
require 'signet/oauth_1/client';
require 'google/api_client';
require 'net/http'
require 'htmlentities'
require 'json'
GOOGLE_CLIENT = Google::APIClient.new(:service => 'translate')

class GoogleTranslate
  attr_accessor :text, :tl
  
  def initialize( sl='en', tl="nl")
    @sl = sl
    @tl = tl
    @uri = URI.parse("http://ajax.googleapis.com/ajax/services/language/translate")
  end
  
  def translate(text=nil)
    @text = text
    translated = ''
    if( not @text.nil? ) and (@text != "" )then
            coder = HTMLEntities.new
            raw = JSON.parse( request );
            if( not raw.nil? ) and not( raw['responseData'].nil? ) then
                    translated = coder.decode( raw['responseData']['translatedText'] )
            end
    end
    puts "translated #{translated}"
    return translated;
  end
  
  private

  def request
          path = "#{@uri.path}?#{params}";
          puts "host |#{@uri.host}| path = |#{path}|"
          Net::HTTP.get(@uri.host, path )
  end
  
  def params
    { :langpair => "#{@sl}|#{@tl}", 
      :q => @text,
      :key => 'AIzaSyAZ3a54okoYxORYFyCYI0rivmha2tAAbbw',
      :v => 1.0 }.map { |k,v| "#{k}=#{CGI.escape(v.to_s)}" }.join('&')
  end
end


TRANSLATOR = GoogleTranslate.new( 'en', 'nl')

def translate(from_lang, to_lang, text)
        return TRANSLATOR.translate( text )
end

# https://www.googleapis.com/language/translate/v2?key=AIzaSyAZ3a54okoYxORYFyCYI0rivmha2tAAbbw&q=flowers&source=en&target=fr&callback=handleResponse
# sudo gem install rack -v 1.2.0
# sudo gem install sinatra liquid signet google-api-client
#
# add language blocks for the descriptions and enumerated type text blocks
# in the given language, initially as copies of the english ('en') blocks.
#
def addLanguageBlocks( filename, language )
        puts "opening #{filename}\n";
        
        doc = REXML::Document.new( File.new(filename) )

        doc.elements.each( "//*/Description" ){
                |description|
                puts "on line ";
                puts " xpath #{description.xpath()}\n"
                lang = description.attributes["lang"];
                if( lang == 'en' ) then
                        parent = description.parent();
                        nl_description = description.clone()
                        nl_description.attributes[ "lang" ] = language
                        if( not description.text().nil? )then
                                nl_description.text = description.text().strip();
                                text = TRANSLATOR.translate( nl_description.text )
                                if( not text.nil? )then
                                        nl_description.text = text;
                                else
                                        nl_description.text = ""
                                end
                        else
                                nl_description.text = '';
                        end
                        parent << nl_description
                end
        }
        
        doc.elements.each( "//*/Text" ){
                |text|
                puts "on text ";
                lang = text.attributes["lang"];
                if( lang == 'en' ) then
                        parent = text.parent();
                        nl_text = text.clone()
                        nl_text.attributes["lang"] = language
                        nl_text.text = text.text().strip(); 
                        nl_text.text = TRANSLATOR.translate( nl_text.text )
                        parent << nl_text
                end
        }
        outfile = File.new( "nl/#{filename}", "w" );
        doc.write( outfile, 3 );
        #outfile.close
end

[ "be_mefisto.xml" ].each{
        |xfile|
        addLanguageBlocks( xfile, 'nl' )        
}
