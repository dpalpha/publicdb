
library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

# https://comtrade.un.org/Data/cache/reporterAreas.json
library("rjson")

s1 <- get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" )
s2 <- get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="2016,2017,2018,2019,2020")

ue28 = c('Austria', 'Bulgaria', 'Croatia', 'Czechia', 'France', 'Germany', 'Greece', 'Hungary', 'Italy', 'Lithuania', 'Luxembourg',
  'Poland','Portugal','Romania','Latvia', 'Slovakia','Slovenia','Spain','Estonia','Netherlands')

africa = c('Algeria','Canary Islands','Ceuta','Egypt','Libya','Madeira','Melilla','Morocco','Tunisia','Western Sahara','Burundi',
           'Comoros','Djibouti','Eritrea','Ethiopia','French Southern Territories','	Kenya','Madagascar','Malawi','Mauritius','Mayotte',
           'Mozambique','Réunion','Rwanda','Seychelles', 'Somalia','Somaliland','South Sudan', 'Sudan','Tanzania','Uganda',
           'Zambia','Zimbabwe','Angola','Cameroon','Central African Republic','Chad','Republic of the Congo','Dem. Rep. of the Congo',
           'Equatorial Guinea','Gabon','Botswana','Eswatini','Lesotho','Namibia','South Africa','Benin','Burkina Faso','Cape Verde',
           'The Gambia','Ghana','Guinea','Guinea-Bissau','Ivory Coast','Liberia','Mali','Mauritania','Niger','Nigeria','Senegal','Sierra Leone','Togo')
southasia = c('Afghanistan','Bangladesh','Bhutan','India','Maldives','Nepal','Pakistan','Sri Lanka')
centralasia = c('Kazakhstan','Kyrgyzstan','Tajikistan','Uzbekistan','Turkmenistan','Azerbaijan')
westernasia = c('Turkey','Bahrain','Kuwait','Oman','Qatar','Saudi Arabia','United Arab Emirates','Yemen','Abkhazia','Armenia',
                'Artsakh','Azerbaijan','Georgia','South Ossetia','Iraq','Israel','Jordan','Lebanon','Palestine','Syria',
                'Iran','Cyprus','Northern Cyprus','Egypt')
eastasia = c('China','Hong Kong','Macau','Japan','Mongolia','North Korea','South Korea','Taiwan','Rep. of Korea')
southeastasia = c('Brunei','Cambodia','East Timor','Indonesia','Laos','Malaysia','Myanmar','Philippines','Singapore','Thailand','Vietnam')
america = c('Anguilla','Antigua and Barbuda','Argentina','Aruba','Bahamas','Belize','Bermuda','Bolivia','Bonaire','Bouvet Island','Brazil',
            'British Virgin Islands','Canada','Cayman Islands','Chile','Clipperton Island','Colombia','Costa Rica','Cuba','Dominica',
            'Ecuador','El Salvador','Falkland Islands','French Guiana','Greenland', 'Grenada','Guadeloupe','Guatemala','Guyana',
            'Haiti','Honduras','Jamaica','Martinique','Mexico','Montserrat','Nicaragua','Panama','Paraguay','Peru','Puerto Rico',
            'Saba','Saint Barthélemy','Saint Kitts and Nevis','Saint Lucia','Saint Martin','Saint Pierre and Miquelon',
            'Saint Vincent and the Grenadines','Sint Eustatius', 'Sint Maarten','South Georgia and South Sandwich Islands', 'Suriname',
            'Trinidad and Tobago','Turks and Caicos Islands', 'USA', 'U.S. Virgin Islands', 'Uruguay','Venezuela')

blacksea = c('Romania', 'Rep. of Moldova','Russian Federation','Georgia','Turkey', 'Bulgaria','Belarus')

dane<-rbind(
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="1991,1992,1993,1994,1995")$data,
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="1996,1997,1998,1999,2000")$data,
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="2001,2002,2003,2004,2005")$data,
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="2006,2007,2008,2009,2010")$data,
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="2011,2012,2013,2014,2015")$data,
  get.Comtrade(r="804", p="all", px='hs', c="100510,100590",ps="2016,2017,2018,2019,2020")$data
  )

library(data.table)
dane$type<-ifelse(dane$ptTitle %like% paste(ue28,collapse = '|'), 'UE',
                  ifelse(dane$ptTitle %like% paste(africa, collapse = '|'),'AFRICA', 
                      ifelse(dane$ptTitle %like% paste(southasia,collapse = '|'),'SOUTH_ASIA',
                        ifelse(  dane$ptTitle %like% paste(centralasia,collapse = '|'),'CENTRAL_ASIA',
                              ifelse(  dane$ptTitle %like% paste(westernasia, collapse = '|'),'WEST_ASIA',  
                                    ifelse(  dane$ptTitle %like% paste(eastasia, collapse = '|'),'EAST_ASIA', 
                                             ifelse(  dane$ptTitle %like% paste(southeastasia,collapse = '|'),'SOATHERN_ASIA', 
                                                      ifelse(  dane$ptTitle %like% paste(america,collapse = '|'),'AMERICA',
                                                               ifelse(  dane$ptTitle %like% paste(blacksea, collapse = '|'),'BLACK_SEA',
                                                                        ifelse(  dane$ptTitle =='WORLD','WORLD', 'ANOTHER'
                                                                                                               
                                                                                                               )          )                                                    
  ))))))))

dane$TradeValue<-as.numeric(dane$TradeValue)

write.csv(dane, 'conference_dane.csv')

