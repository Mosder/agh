from fastapi import FastAPI, status
from fastapi.responses import HTMLResponse, JSONResponse
import os
import signal
from dotenv import load_dotenv
import requests

load_dotenv()
API_KEY = os.environ.get("API_KEY")
if API_KEY is None:
    print("API_KEY environment variable not specified. Shutting down server...")
    os.kill(os.getppid(), signal.SIGINT)
    exit()

VANITY_URL_REQUEST_URL = f"https://api.steampowered.com/ISteamUser/ResolveVanityURL/v0001/?key={API_KEY}&vanityurl={{vanity_url}}"
USER_INFO_REQUEST_URL = f"https://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key={API_KEY}&steamids={{steam_id}}"
GAMES_REQUEST_URL = f"https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key={API_KEY}&steamid={{steam_id}}"
ACHIEVEMENTS_REQUEST_URL = f"https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v0001/?key={API_KEY}&steamid={{steam_id}}&appid={{game_id}}"

app=FastAPI()

# home page form
@app.get("/")
async def get_home_page():
    with open("index.html", "r") as f:
        home_page = f.read()
    return HTMLResponse(content=home_page, status_code=status.HTTP_200_OK)

# return error page
def error_page(status_code: int, error_reason: str, error_message: str):
    with open("error.html", "r") as f:
        error_page = f.read()
    error_page = (
        error_page.replace("{ERROR_CODE}", str(status_code))
        .replace("{ERROR_REASON}", error_reason)
        .replace("{ERROR_MESSAGE}", error_message)
    )
    return HTMLResponse(content=error_page, status_code=status_code)

# return API error page
def api_error_page(status_code: int, error_reason: str, error_response: str):
    with open("api_error.html", "r") as f:
        error_page = f.read()
    error_page = (
        error_page.replace("{ERROR_CODE}", str(status_code))
        .replace("{ERROR_REASON}", error_reason)
        .replace("{ERROR_RESPONSE}", error_response)
    )
    return HTMLResponse(content=error_page, status_code=status_code)

# make GET requests
# returns tuple of (True, response_json) if request was successful
#      or tuple of (False, error_page) if it wasn't
def get_request(url: str, ignore_error_fun = lambda res: False):
    response = requests.get(url)
    # ignore error for games with no achievements
    if response.status_code == 200 or ignore_error_fun(response):
        return (True, response.json())
    else:
        error_page = api_error_page(response.status_code, response.reason, response.text)
        return (False, error_page)

# class of user
class User:
    def __init__(self, steam_id, username, avatar_url):
        self.steam_id = steam_id
        self.username = username
        self.avatar_url = avatar_url

# class of game
class Game:
    def __init__(self, game_id, play_time_mins):
        self.game_id = game_id
        self.name = None
        self.play_time = play_time_mins
        self.achievements_completed = None
        self.achievements_total = None

    def completion_percentage(self):
        return 100 * self.achievements_completed / self.achievements_total

# get steamid from vanity url
# returns tuple of (True, steamid) if request was successful
#      or tuple of (True, None) if request was successful, but no match
#      or tuple of (False, error_page) if request wasn't sucessful
def get_steam_id(vanity_url: str):
    request_url = VANITY_URL_REQUEST_URL.format(vanity_url=vanity_url)
    ok, response = get_request(request_url)
    # API server error
    if not ok:
        return (False, response)

    ok = response["response"]["success"]
    # no match
    if ok != 1:
        return (True, None)

    return (True, response["response"]["steamid"])

# get user info from steamid
# returns tuple of (True, user_class) if request was successful
#      or tuple of (False, error_page) if it wasn't
def get_user_info(steam_id: str):
    request_url = USER_INFO_REQUEST_URL.format(steam_id=steam_id)
    ok, response = get_request(request_url)
    # API server error
    if not ok:
        return (False, response)

    players_list = response["response"]["players"]
    # bad steamid error
    if len(players_list) < 1:
        return (False, error_page(404, "Not Found", "Couldn't find user of given VanityURL ID / Steam64 ID"))

    player = players_list[0]
    user = User(player["steamid"], player["personaname"], player["avatarfull"])
    return (True, user)

# get list of games of user
# returns tuple of (True, list_of_games) if request was successful
#      or tuple of (False, error_page) if it wasn't
def get_games_list(steam_id: str):
    request_url = GAMES_REQUEST_URL.format(steam_id=steam_id)
    ok, response = get_request(request_url)
    # API server error
    if not ok:
        return (False, response)

    # private profile / private games
    if "games" not in response["response"]:
        return (False, error_page(403, "Forbidden", "Profile is private / User has privatted game list"))
    games_list = response["response"]["games"]
    # filter games with 0 playtime
    games_list = [game for game in games_list if game["playtime_forever"] > 0]
    # if game list is empty
    if len(games_list) < 1:
        return (False, error_page(404, "Not Found", "Couldn't find any games for given user"))

    games_classes = [Game(game["appid"], game["playtime_forever"]) for game in games_list]
    # sort by playtime
    def sort_func(g: Game):
        return g.play_time
    games_classes = sorted(games_classes, key=sort_func, reverse=True)
    return (True, games_classes)

# get game info by game id and steam_id
# returns tuple of (True, updated_game_class) if request was successful
#      or tuple of (True, None) if request was successful, but game has no achievements
#      or tuple of (False, error_page) if it wasn't
def get_game_info(steam_id: str, game: Game):
    request_url = ACHIEVEMENTS_REQUEST_URL.format(steam_id=steam_id, game_id=game.game_id)
    # ignore jumping to error page when game has no achievements
    ignore_error_lambda = lambda res: "playerstats" in res.json()
    ok, response = get_request(request_url, ignore_error_lambda)
    # API server error
    if not ok:
        return (False, response)

    stats = response["playerstats"]
    # game has no achievements (or is some other app, e.g. Proton)
    if not stats["success"] or "achievements" not in stats:
        return (True, None)

    game.name = stats["gameName"]
    achievements = stats["achievements"]
    game.achievements_total = len(achievements)
    game.achievements_completed = len([ach for ach in achievements if ach["achieved"] == 1])
    return (True, game)

# get table rows for games
def get_games_table_rows(games: list[Game]):
    row_template = "<tr><td>{no}</td><td>{id}</td><td>{name}</td><td>{playtime}</td><td>{achievements}</td></tr>"
    result_str = ""
    for i, game in enumerate(games):
        no = f"{i+1}."
        id = game.game_id
        name = game.name
        playtime_hrs = game.play_time // 60 # play time in hrs
        playtime_mins = game.play_time % 60 # remainder in mins
        playtime = f"{playtime_hrs}hrs {playtime_mins}mins"
        achievements = f"{game.achievements_completed} / {game.achievements_total} ({game.completion_percentage():.2f}%)"
        result_str += row_template.format(no=no, id=id, name=name, playtime=playtime, achievements=achievements)
    return result_str

# get page of list of games
def get_game_list_page(user: User, games: list[Game]):
    with open("result.html", "r") as f:
        result_page = f.read()

    game_count = len(games)
    playtime_sum = sum(game.play_time for game in games)
    playtime_sum_str = f"{playtime_sum // 60}hrs {playtime_sum % 60}mins"
    achievements_completed_sum = sum(game.achievements_completed for game in games) 
    achievements_total_sum = sum(game.achievements_total for game in games) 
    achievements_percentage_sum = 100 * achievements_completed_sum // achievements_total_sum
    achievements_sum_str = f"{achievements_completed_sum} / {achievements_total_sum} ({achievements_percentage_sum:.2f}%)"

    result_page = (
        result_page.replace("{AVATAR_URL}", user.avatar_url)
        .replace("{USER_NAME}", user.username)
        .replace("{STEAM_ID}", user.steam_id)
        .replace("{GAME_COUNT}", str(game_count))
        .replace("{PLAYTIME_SUM}", playtime_sum_str)
        .replace("{ACHIEVEMENTS_SUM}", achievements_sum_str)
        .replace("{GAMES_ROWS}", get_games_table_rows(games))
    )
    return HTMLResponse(content=result_page, status_code=status.HTTP_200_OK)

# achievements data response
@app.get("/achievements")
async def results(user_id: str, game_limit: int):
    # validate game limit
    if game_limit < 1:
        return error_page(400, "Bad Request", "Game limit should be at least 1")

    # get steamid if input is vanity url
    ok, response = get_steam_id(user_id)
    if not ok:
        return response
    if response is not None:
        user_id = response

    # get user info class
    ok, response = get_user_info(user_id)
    if not ok:
        return response
    user = response

    # get games list
    ok, response = get_games_list(user.steam_id)
    if not ok:
        return response
    games = response

    # update game infos by the name and achievements info
    updated_games = []
    info_gathered_count = 0
    for game in games:
        ok, response = get_game_info(user.steam_id, game)
        if not ok:
            return response
        # take only games with achievements
        if response is not None:
            updated_games.append(response)
            info_gathered_count += 1
            # don't go over game limit
            if info_gathered_count >= game_limit:
                break

    # return error if no games
    if len(updated_games) < 1:
        return error_page(404, "Not Found", "Couldn't find any games with achievements for given user / User has game info hidden")

    # return game list page
    return get_game_list_page(user, updated_games)

