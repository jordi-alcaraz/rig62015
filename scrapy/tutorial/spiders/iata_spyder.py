import scrapy, urllib, urllib2
import csv
from tutorial.items import IATA_name


class stateSpider(scrapy.Spider):
    name= "state"
    allowed_domains = ["en.wikipedia.org"]
    start_urls = ["http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"]
    def parse(self, response):
        ofile  = open('states_cities.csv', "w")
        writer = csv.writer(ofile)
        item = IATA_name()
        head_out = []
        head_out.append("State")
        head_out.append("State_code")
        writer.writerow(head_out)
        for row in response.xpath("//table[contains(@class,'wikitable sortable')]/tr[not(contains(@style,'ffffbb')) and not(contains(@style,'ddeeff')) and not(contains(@style,'ddffdd'))]"):
            row_out = []
            item['state'] = row.xpath('td[1]/a/text()').extract()
            item['IATA_code'] = row.xpath('td[6]/span/text()').extract()
            if len(item['state'])>0:
                row_out.append(item['state'][0])
                row_out.append(item['IATA_code'][0])
                writer.writerow(row_out)
        ofile.close()


class IATASpider(scrapy.Spider):
    name = "IATA"
    allowed_domains = ["airportcodes.us"]
    start_urls = ["http://www.airportcodes.us/us-airports-by-state.htm"]
    def parse(self, response):
        code_state = []
        with open('states_cities.csv', 'rb') as f:
           reader = csv.reader(f)
           for row in reader:
              code_state.append(row)
        f.close() 
        ofile  = open('IATA_cities.csv', "w")
        writer = csv.writer(ofile)
        item = IATA_name()
        head_out = []
        head_out.append("State_code")
        head_out.append("State")
        head_out.append("City")
        head_out.append("IATA")
        writer.writerow(head_out)
        for row in response.xpath('//table[@class="c"]/tr'):
            row_out = []
            item['state'] = row.xpath('td[1][not(@colspan="4")]/text() | th[1][not(@colspan="4")]/text()').extract()
            item['city'] = row.xpath('td[2][not(@colspan="4")]/text() | th[2][not(@colspan="4")]/text()').extract()
            item['IATA_code'] = row.xpath('td[3][not(@colspan="4")]/text() | th[3][not(@colspan="4")]/text()').extract()
            if len(item['state'])>0:
                for code in code_state:
                    if code[1] == item['state'][0]:
                        row_out.append(item['state'][0])
                        item['state'][0] = code[0]
                        row_out.append(item['state'][0])
                        row_out.append(item['city'][0])
                        row_out.append(item['IATA_code'][0])
                        writer.writerow(row_out)
        ofile.close()
