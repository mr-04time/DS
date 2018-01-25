#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 32 17:01:05 2018
@author: rehan
In the script we are retrieving  user entered movie name, creating a sorted list for that we fetch data from an API which is in json format
"""
from urllib.request import urlopen as uo
import json

def get_movie_list(substr):
    url='https://jsonmock.hackerrank.com/api/movies/search/'
    # opening connection reading the page
    uClient=uo(url)
    json_movie=uClient.read()
    uClient.close()
    # to work with json
    parsed_json = json.loads(json_movie)
    tot_page=parsed_json["total_pages"]
    # movie name to search
    search_movie=substr
    movie_list=parsed_json['data']
    # creating a array name title
    title=[]

    for pageNo in range(0,tot_page):
        mod_url='https://jsonmock.hackerrank.com/api/movies/search/?Title='+str(search_movie)+'&page='+str(pageNo)
        uClient=uo(mod_url)
        json_searchMovie=uClient.read()
        uClient.close()
        #to work with json
        json_movie = json.loads(json_searchMovie)
        movie_list=json_movie['data']

        for i in range(0,len(movie_list)):
            title.append(movie_list[i]['Title'])
            #print(movie_list[i]['Title'])
            
    return sorted(title)



# using the function to get the movie name
# print(get_movie_list('battle'))



    



