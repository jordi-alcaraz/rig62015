# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class IATA_name(scrapy.Item):
    # define the fields for your item here like:
    city = scrapy.Field()
    state = scrapy.Field()
    IATA_code = scrapy.Field()
    city_link = scrapy.Field()
    link = scrapy.Field()
