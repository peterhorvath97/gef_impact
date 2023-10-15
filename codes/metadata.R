library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)

metadata <- readRDS(file.path('data', 'metadata.rds'))

metadata <- metadata %>% 
  filter(frequency != 'A') %>% 
  select(database, code, frequency, descriptor, numobs, datatype, aggtype) %>% 
  distinct(code, .keep_all = TRUE)

metadata <- metadata %>% 
  filter(numobs >= 50,
         database != 'crypto',
         database != 'esg',
         database != 'bondindx') %>% 
  mutate(descriptor_lower = tolower(descriptor)) %>% 
  filter(!str_detect(descriptor_lower, or('interest','bank', 'asset', 'liab',  'policy', 'repo', 'fdi', 'rent', 'ton', 'kg', 'volatility',
                                    'mortgage', 'hous', 'liq', 'gov', 'estate', 'resid', 'prop', 'home', 'permit', 'inhab', 'mother', 
                                    'int rate', 'money', 'market', 'mkt', 'supply', 'demand', 'ast', 'asst', 'equity', 'eqt', 'basket',
                                    'trade bal','duration', 'cpi', 'consumer', 'stoxx', 'topix', 'cboe', 'imf', 'fbr', 'sentiment',
                                    'gap', 'productivity', 'value added', 'reserve', 'cred', 'incom', 'portfolio', 'p/e', 'stat', 'o/n',
                                    'wage', 'salar', 'emp', 'activ', 'gva', 'lab', 'job', 'work', 'vacan', 'earn', 'wg', 'hrs', 'wkly',
                                    'exchange', 'foreign currency', 'ret tr', 'borrow', 'lend', 'lns', 'yr', 'fin', 'reit', 'dividend',
                                    'weight', 'wght', 'cost', 'core', 'chg', 'change', '%', 'wts', 'hlpi', 'overnight', 'wall street',
                                    'stock exchange', 'stock market', 'stock', 'bond', 'loan', 'share', 'shr', 'gold', 'silver', 'advance',
                                    'treasury', 'return', 'risk', 'yield', 'yld', 'fwd', 'forward', 'dwell', 'lost', 'announce', 'kase',
                                    'production', 'budget', 'deficit', 'rev', 'deb', 'sec', 'dbt', 'econ', 'defl', 'external', 'arriv',
                                    'vehicle', 'tour', 'health', 'trans', 'service', 'spread', 'level', 'condo', 'ip', 'bloomberg',
                                    'flash', 'department', 'car', 'education', 'retail', 'sale', 'excl', 'single', 'spix', 'dow jones',
                                    'dep', 'deposit', 'corp', 'issue', 'fund', 'constr', 'settle', 'durable', 'gdp', 'air', 'curr',
                                    'profit', 'surplus', 'consump', 'inv', 'current account', 'discount', 'moex', 'holding', 'hldg', 'dose',
                                    'tax', 'subsid', 'cap', 'goods', 'covid', 'med', 'legal', 'manag', 'mgm', 'gfcf', 'exch', 'recess',
                                    'turnover', 'payment', 'bk', 'bnk', 'collat', 'posit', 'adj', 'domestic', 'pce', 'volume', 'convers',
                                    'commerc', 'ins', 'operat', 'not', 'misc', 'loss', 'passanger', 'travel', 'double', 'fx intervention',
                                    'lci', 'underwriting', 'grant', 'contrib', 'asyl', 'error', 'net', 'national', 'saving', 'spot', 'long', 'short',
                                    'terms', 'tot', 'discrep', 'capac', 'disp', 'out', 'uncertain', 'test', 'deg', 'crb', 'acc', 'alberta',
                                    'bus', 'zillow', 'btos', 'apartm', 'prem', 'futur', 'constant', 'cash', 's&p', 'fomc', 'eer', 'cross',
                                    'hicp', 'peak', 'ind', 'deriv', 'monetary', 'population', 'occup', 'customer', 'build', 'pension',
                                    'foreigner', 'register', 'belex', 'disc', 'approv', 'cum', 'order', 'new', 'compensate', 'fsi', 'ous',
                                    'compan', 'person', 'coef', 'pass', 'auction', DGT %R% ' day', DGT %R% '-month', 'matur', 'parity', 'storage',
                                    'arrear', 'minis', 'hfce', 'ct:', 'nsw', 'terr', 'interven', 'generat', 'offer', 'rate', 'bill', 
                                    'survey', 'ftse', 'archived', DGT %R% '-day', DGT %R% ' month', 'infl',
                                    'purchase', 'time', 'check', 'bid', 'm' %R% DGT, 'steril', 'frb', 'power load', 'power average', 'regis', 'history',
                                    'abbotsford', 'hotel', 'dhabi', 'abru', 'aero', 'ab:', 'aber', 'life', 'claim', 'rights', 'alloc', 'number', 'bop',
                                    'duty', 'pers', 'hospital', 'way', 'city' ))) %>% 
  select(-descriptor_lower)


concentration <- metadata %>% 
  filter((str_detect(descriptor, 'Imports') & str_detect(descriptor, 'from')) | (str_detect(descriptor, 'Exports') & str_detect(descriptor, 'to')))

  
  #metadata %>% 
  #filter(str_detect(descriptor, or('Imports from', 'Exports to')))


prices <- metadata %>% 
  filter(str_detect(descriptor, or('Export Price', 'Import Price')))



concentration <- concentration %>% 
  #Get the 'country of origin' and clean up
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, SPACE %R% or('E', 'I'))[, 'start']),
         descriptor = str_remove_all(descriptor, country),
         country = country %>%  
           str_remove_all(':') %>% 
           str_remove_all(SPACE %R% END) %>% 
           str_remove_all(DOT)) %>% 
  mutate(country = ifelse(str_detect(country, 'Congo'), 'Congo', country),
         country = ifelse(str_detect(country, 'Bosnia'), 'Bosnia & Herzegovina', country),
         country = ifelse(str_detect(country, 'Czech'), 'Czech Republic', country),
         country = ifelse(str_detect(country, 'Yemen'), 'Yemen', country),
         country = ifelse(str_detect(country, 'Vincent'), 'St Vincent', country),
         country = ifelse(str_detect(country, 'S African'), 'S Afr Common Custom Area', country),
         country = ifelse(str_detect(country, 'S Afr'), 'South Africa', country),
         country = ifelse(str_detect(country, 'Latvia'), 'Latvia', country),
         country = ifelse(str_detect(country, 'Brune'), 'Brunei', country),
         country = ifelse(str_detect(country, 'United Kingdom'), 'UK', country),
         country = ifelse(str_detect(country, 'Dominica'), 'Dominican Republic', country),
         country = ifelse(str_detect(country, 'Euro Area 19'), 'EA19', country),
         country = ifelse(str_detect(country, 'Slovak'), 'Slovakia', country),
         country = ifelse(str_detect(country, 'Cote'), 'Cote d\'Ivoire', country),
         country = ifelse(str_detect(country, 'Trinidad'), 'Trinidad & Tobago', country),
         country = ifelse(str_detect(country, 'China'), 'China', country),
         country = ifelse(str_detect(country, 'USSR'), 'Russia', country),
         country = ifelse(str_detect(country %>% tolower, 'swatini'), 'Eswatini', country),
         country = ifelse(str_detect(country, 'Lucia'), 'St Lucia', country),
         country = ifelse(str_detect(country, 'Kitts'), 'St Kitts & Nevis', country),
         country = ifelse(str_detect(country, 'Viet'), 'Vietnam', country),
         country = ifelse(str_detect(country, 'Macedonia'), 'Macedonia', country),
         country = ifelse(str_detect(country, 'Bissau'), 'Guinea-Bissau', country),
         country = ifelse(str_detect(country %>% tolower, 'swatini'), 'Eswatini', country),
         country = ifelse(str_detect(country, 'Swaziland'), 'Eswatini', country),
         country = ifelse(str_detect(country, 'Lucia'), 'St Lucia', country),
         country = ifelse(str_detect(country, 'Kitts'), 'St Kitts & Nevis', country),
         country = ifelse(str_detect(country, 'Maarten'), 'St Martin', country),
         country = ifelse(str_detect(country, 'British') & str_detect(country, 'Virgin'), 'British Virgin Islands', country),
         country = ifelse(str_detect(country, 'US') & str_detect(country, 'Virgin'), 'US Virgin Islands', country),
         country = ifelse(!str_detect(country, 'US') & !str_detect(country, 'British') & str_detect(country, 'Virgin'), 'US Virgin Islands', country),
         country = ifelse(str_detect(country, 'Heard'), 'Heard & McDonald Islands', country),
         country = ifelse(str_detect(country, 'Helena'), 'St Helena Island', country),
         country = ifelse(str_detect(country %>% tolower, 'lao' %R% optional('s')), 'Laos', country),
         country = ifelse(country == 'Korea', 'South Korea', country),
         country = str_replace_all(country, 'oil', 'Oil'),
         country = str_replace_all(country, ' and', ' &'),
         country = str_replace_all(country, 'Dev ', 'Developing '),
         country = str_replace_all(country, 'countries', 'Countries'),
         country = str_replace_all(country, 'Devloping', 'Developing'),
         country = str_replace_all(country, 'Rep' %R% END, 'Republic'),
         country = str_replace_all(country, 'Eq ', 'Equatorial '),
         country = str_replace_all(country, 'Saint', 'St'),
         country = str_replace_all(country, 'AU', 'Australia'),
         country = str_replace_all(country, 'Afr ', 'Africa '),
         country = str_replace_all(country, 'Cent ', 'Central '),
         country = str_remove_all(country, ' PDR'),
         country = str_remove_all(country, '/.*'),
         country = ifelse(str_detect(country, 'Central') & str_detect(country, 'Africa'), 'Central African Republic', country)
         ) %>% 
  drop_na() %>% 
  filter(!str_detect(country, or('Export', 'Import')),
         !str_detect(country, or('Developing', 'Oil', 'EA', 'EZ', 'EU', 'Euro', 'Other', 'Hemisphere', 
                                 'CIS', 'World', 'Australia ', 'Sub', 'Reunion' )),
         country != "Africa") %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  filter(n > 50) %>% 
  select(-n) %>% 
  #Remove goods specific exports/imports
  filter(!str_detect(descriptor, ':')) %>% 
  #Retrieve currency, seasonal adjutment, etc
  mutate(braccontent = str_extract_all(descriptor, '\\([^)]*\\)') %>% as.character() %>% 
           str_remove_all('"(.*?)"\\s*,') %>% 
           str_remove_all('c\\(') %>% 
           str_remove_all('"\\)') %>% 
           str_remove_all('"'),
         descriptor = str_remove_all(descriptor, braccontent),
         braccontent = braccontent %>% 
           str_remove_all('\\(') %>% 
           str_remove_all('\\)'))  %>% 
  filter(!str_detect(braccontent, 'YTD')) %>% 
  mutate(seas = str_extract_all(braccontent, '^[^,]*') %>% as.character(),
         seas = ifelse(str_detect(seas, DOT), NA, seas),
         braccontent = ifelse(!is.na(seas), str_remove_all(braccontent, seas), braccontent) %>% 
           str_remove_all(',' %R% optional(SPACE) ),
         seas = ifelse(str_detect(seas, 'SWDA'), 'SA', seas),
         seas = ifelse(str_detect(seas,'NSA'), 'NSA', seas),
         seas = replace_na(seas, 'NSA'),
         milbil = str_extract(braccontent, or('Mil', 'Bil')),
         braccontent = braccontent %>% 
           str_remove_all(milbil) %>% 
           str_remove_all(DOT) %>% 
           str_replace_all(DOLLAR, 'D') %>% 
           str_remove_all('s' %R% END),
         braccontent = ifelse(str_detect(tolower(braccontent), 'eur'), 'EUR', braccontent),
         braccontent = ifelse(braccontent == 'D', 'USD', braccontent)) %>% 
  filter(!is.na(milbil)) %>% 
  #Have the observations with the most amount of common currency kept only
  rename(currency = braccontent) %>% 
  group_by(country, currency) %>% 
  mutate(n = n()) %>% 
  ungroup(currency) %>% 
  filter(n == max(n)) %>% 
  select(-n) %>% 
  ungroup() %>% 
  #Create partner countries
  mutate(partner = descriptor %>% 
           str_extract_all(or('from', 'to') %R% '.*') %>% as.character() %>% 
           str_remove_all('\\([^)]*\\)') %>% 
           str_remove_all(or('from', 'to', '\\(\\)')) %>% 
           str_remove_all(or(SPACE %R% END, START %R% SPACE)) %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(',')) %>% 
  mutate(partner = ifelse(str_detect(partner, 'Congo'), 'Congo', partner),
         partner = ifelse(str_detect(partner, 'Bosnia'), 'Bosnia & Herzegovina', partner),
         partner = ifelse(str_detect(partner, 'Czech'), 'Czech Republic', partner),
         partner = ifelse(str_detect(partner, 'Yemen'), 'Yemen', partner),
         partner = ifelse(str_detect(partner, 'Vincent'), 'St Vincent', partner),
         partner = ifelse(str_detect(partner, 'S African'), 'S Afr Common Custom Area', partner),
         partner = ifelse(str_detect(partner, 'S Afr'), 'South Africa', partner),
         partner = ifelse(str_detect(partner, 'Latvia'), 'Latvia', partner),
         partner = ifelse(str_detect(partner, 'Brune'), 'Brunei', partner),
         partner = ifelse(str_detect(partner, 'United Kingdom'), 'UK', partner),
         partner = ifelse(str_detect(partner, 'Dominica'), 'Dominican Republic', partner),
         partner = ifelse(str_detect(partner, 'Euro Area 19'), 'EA19', partner),
         partner = ifelse(str_detect(partner, 'Slovak'), 'Slovakia', partner),
         partner = ifelse(str_detect(partner, 'Trinidad'), 'Trinidad & Tobago', partner),
         partner = ifelse(str_detect(partner, 'China'), 'China', partner),
         partner = ifelse(str_detect(partner, 'USSR'), 'Russia', partner),
         partner = ifelse(str_detect(partner %>% tolower, 'swatini'), 'Eswatini', partner),
         partner = ifelse(str_detect(partner, 'Swaziland'), 'Eswatini', partner),
         partner = ifelse(str_detect(partner, 'Lucia'), 'St Lucia', partner),
         partner = ifelse(str_detect(partner, 'Kitts'), 'St Kitts & Nevis', partner),
         partner = ifelse(str_detect(partner, 'Maarten'), 'St Martin', partner),
         partner = ifelse(str_detect(partner %>% tolower, 'lao' %R% optional('s')), 'Laos', partner),
         partner = ifelse(str_detect(partner, 'British') & str_detect(partner, 'Virgin'), 'British Virgin Islands', partner),
         partner = ifelse(str_detect(partner, 'US') & str_detect(partner, 'Virgin'), 'US Virgin Islands', partner),
         partner = ifelse(!str_detect(partner, 'US') & !str_detect(partner, 'British') & str_detect(partner, 'Virgin'), 'US Virgin Islands', partner),
         partner = ifelse(str_detect(partner, 'Heard'), 'Heard & McDonald Islands', partner),
         partner = ifelse(str_detect(partner, 'Helena'), 'St Helena Island', partner),
         partner = ifelse(str_detect(partner, 'Viet'), 'Vietnam', partner),
         partner = ifelse(str_detect(partner, 'Russia'), 'Russia', partner),
         partner = ifelse(str_detect(partner, 'Venezuela'), 'Venezuela', partner),
         partner = ifelse(str_detect(partner, 'Bulgaria'), 'Bulgaria', partner),
         partner = ifelse(str_detect(partner, 'Rico'), 'Puerto Rico', partner),
         partner = ifelse(str_detect(partner, 'Moldova'), 'Moldova', partner),
         partner = ifelse(str_detect(partner, 'Maldives'), 'Maldives', partner),
         partner = ifelse(str_detect(partner, 'Tanzania'), 'Tanzania', partner),
         partner = ifelse(str_detect(partner, 'Iran'), 'Iran', partner),
         partner = ifelse(str_detect(partner, 'Beligum'), 'Belgium', partner),
         partner = ifelse(str_detect(partner, 'Great Britain'), 'UK', partner),
         partner = ifelse(str_detect(partner, 'Macedonia'), 'Macedonia', partner),
         partner = ifelse(str_detect(partner %>% tolower, 'timor'), 'Timor-Leste', partner),
         partner = ifelse(str_detect(partner, 'Bissau'), 'Guinea-Bissau', partner),
         partner = ifelse(str_detect(descriptor, 'Korea, PDR'), 'North Korea', partner),
         partner = ifelse(str_detect(partner, 'Cote'), 'Cote d\'Ivoire', partner),
         partner = ifelse(str_detect(partner, 'Cent') & str_detect(partner, 'Afr'),'Central African Republic', partner),
         partner = ifelse(partner == 'Korea', 'South Korea', partner),
         partner = ifelse(str_detect(partner, 'Korea') & str_detect(partner %>% tolower, 'people'), 'North Korea', partner),
         partner = ifelse(str_detect(partner, 'Korea') & str_detect(partner %>% tolower, 'republic of'), 'South Korea', partner),
         partner = ifelse(partner == 'USA', 'US', partner),
         partner = ifelse(partner == 'America', 'US', partner),
         partner = str_replace_all(partner, 'oil', 'Oil'),
         partner = str_replace_all(partner, ' and', ' &'),
         partner = str_replace_all(partner, 'Dev ', 'Developing '),
         partner = str_replace_all(partner, 'countries', 'Countries'),
         partner = str_replace_all(partner, 'Devloping', 'Developing'),
         partner = str_replace_all(partner, or('Rep' %R% END, 'Repub '), 'Republic'),
         partner = str_replace_all(partner, or('Eq ', 'Equarial '), 'Equatorial '),
         partner = str_replace_all(partner, 'Saint', 'St'),
         partner = str_replace_all(partner, 'AU', 'Australia'),
         partner = str_replace_all(partner, 'Afr ', 'Africa '),
         partner = str_replace_all(partner, 'Cent ', 'Central '),
         partner = str_remove_all(partner, ' PDR'),
         partner = str_remove_all(partner, '/.*'),
         partner = str_remove_all(partner, START %R% 'the '), 
         partner = str_remove_all(partner, ' \\[(.*?)\\]'),
         partner = str_remove_all(partner, optional('The ') %R% 'Republic ' %R% 'of ')
  ) %>% 
  filter(!str_detect(partner, or('Developing', 'Oil', 'EA', 'EZ', 'EU', 'Euro', 'Other', 'Hemisphere', 'CIS', 
                                 'World', 'Countries', 'Asia', 'Special', '& Oceania', 'Africa ns', 'Sub', 'Reunion')),
         partner != 'Africa') %>% 
  group_by(partner) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  select(-n) %>% 
  #Distinguish btw export and import
  mutate(expimp = ifelse(str_detect(descriptor,  'Exports'), 'Exports', 'Imports')) %>% 
  filter(aggtype == 'SUM',
         frequency == 'M',
         seas != 'SA') %>% 
  #Note that Japan is in Bn Yen and not Mn like other countries
  select(country, expimp, partner, country, currency, numobs, database, code) %>% 
  group_by(country, expimp, partner, currency) %>% 
  mutate(n = n()) %>% 
  filter(n == 1 | n > 1 & numobs == max(numobs)) %>% 
  distinct(country, expimp, partner, currency, .keep_all = TRUE) %>% 
  select(-n, -numobs)

saveRDS(concentration, file.path('data', 'concentration_codes.rds'))



prices %>% 
#Get the 'country of origin' and clean up
mutate(country = substr(descriptor, 
                        start = 1, 
                        stop = str_locate(descriptor, SPACE %R% or('E', 'I'))[, 'start']),
       descriptor = ifelse(!is.na(country), str_remove_all(descriptor, country), descriptor) %>% 
         str_remove_all(SPACE %R% END) %>% 
         str_remove_all(START %R% SPACE),
       country = country %>%  
         str_remove_all(':') %>% 
         str_remove_all(SPACE %R% END) %>% 
         str_remove_all(START %R% SPACE) %>% 
         str_remove_all(DOT),
       country = ifelse(database == 'usint', 'US', country),
       expimp = str_extract(descriptor, START %R% '.*' %R%  'Price' %R% optional('s')),
       descriptor = str_remove_all(descriptor, expimp) %>% 
         str_remove_all(START %R% or(SPACE, optional(SPACE) %R% or(': ', ', ') )),
       expimp = str_extract(expimp, or('Export', 'Import') ))  %>% 
  filter(!is.na(expimp)) %>%  
  mutate(country = ifelse(str_detect(country, 'Czech'), 'Czech Republic', country),
         country = ifelse(str_detect(country, 'Korea'), 'Korea', country),
         country = ifelse(str_detect(country, 'Thai'), 'Thailand', country),
         country = ifelse(str_detect(country, 'Canada'), 'Canada', country)) %>% 
  filter(!str_detect(country, or('EA', 'EU', 'Euro'))) %>%
  mutate(brackets = str_extract_all(descriptor, '\\([^)]*\\)') %>% as.character(),
         descriptor = str_remove_all(descriptor, brackets) %>% 
           str_remove_all(or('\\(', '\\)')) %>% 
           str_remove_all(SPACE %R% END),
         brackets = brackets %>% 
           str_remove_all(or('\\(', '\\)')),
         seas = ifelse(str_detect(brackets, 'NSA'), 'NSA', NA ),
         seas = ifelse(is.na(seas) & str_detect(brackets, 'SA'), 'SA', 'NSA' ),
         brackets = str_remove_all(brackets, seas) %>% 
           str_remove_all(',' %R% optional(SPACE))) %>% 
  mutate(descriptor = descriptor %>% 
           str_remove_all('.*' %R% optional(SPACE) %R% ':') %>% 
           str_remove_all(START %R% SPACE) %>% 
           str_remove_all('\\[(.*?)\\]') %>% 
           str_remove_all(SPACE %R% END) ) %>% 
  filter(!str_detect(descriptor, or('Oth', 'oth'))) %>% 
  janitor::tabyl(descriptor) %>% arrange(desc(n)) %>% drop_na()





