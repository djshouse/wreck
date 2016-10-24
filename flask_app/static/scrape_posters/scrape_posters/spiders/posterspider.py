from scrape_posters.items import Poster
import csv
import scrapy

class PosterSpider(scrapy.Spider):
    name = 'pyimagesearch-poster-spider'
    with open('url_list.csv', newline = '') as f:
    	start_urls = list(f.read().splitlines())
    
    def parse(self, response):
        img = response.css('.poster a img').xpath('@src')
        imageURL = img.extract_first()
        title = response.css('.poster a img').xpath('@title').extract()
        yield Poster(title = title, image_urls = [imageURL])
