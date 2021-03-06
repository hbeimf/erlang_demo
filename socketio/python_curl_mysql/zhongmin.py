#coding=utf-8
import sys
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
# from selenium.webdriver.common.action_chains import ActionChains #引入ActionChains鼠标操作类
import time
import re

import html2text

import json

from lib.MySQL import MySQL

"""
huize | 中民

"""

class FetchWeb :
    def __init__(self):
        print("start...")
        reload(sys)                         # 2
        sys.setdefaultencoding('utf-8')
        self.db = MySQL()
        self.init_browser()

    def __del__(self) :
        self.browser.quit()
        print("close")

    def init_browser(self):
        self.browser = webdriver.Firefox()
        # self.browser = webdriver.PhantomJS()
        # self.browser = webdriver.Chrome()
        # 保存主窗口句柄， id 号
        self.main_window_handle = self.browser.current_window_handle


    def reboot_browser(self):
        self.browser.quit()
        self.init_browser()


    def run(self) :
        self.zhongmin_go()
        # self.reboot_browser()
        self.zhongmin_go_v2()


    def save(self, name, category, row):
        print name
        print category
        print row



    # 中民 销量
    def zhongmin_go(self):
        url = "http://www.zhongmin.cn/health/health-1g-1m-1o63s-1c-1e-1od1ot1bpg1.html"
        self.open_window(url)
        self.zhongmin_gogo("中民", "销量")


    # 中民 人气
    def zhongmin_go_v2(self):
        url = "http://www.zhongmin.cn/health/health-1g-1m-1o63s-1c-1e-1od2ot1bpg1.html"
        self.open_window(url)
        self.zhongmin_gogo("中民", "人气")


    def zhongmin_gogo(self, name, category):
        rows = self.zhongmin_find_data(name, category)

        # print "++++++++++++++++++++++++++++++++++++++++++++++++"



        # for row in rows:
        #     # print row['name']
        #     # print row['link']
        #     # print row['company']
        #     # print row['sales_volume']
        #     # print row['comment_number']
        #     # print row['minimum_premium']

        #     # # time.sleep(2)
        #     # # row['underwriting_age'] = reply['underwriting_age']
        #     # # row['guarantee_period'] = reply['guarantee_period']

        #     # print row['underwriting_age']
        #     # print row['guarantee_period']

        #     row['website_name'] = name
        #     row['order_type'] = category
        #     self.db.insert('bx_list', row)

            # self.save(name, category, row)
        time.sleep(1)

    def zhongmin_find_data(self, website_name, order_type):
        rows = []

        lis = self.browser.find_elements_by_class_name("list_pro_frame")

        # print len(lis)
        for li in lis:
            data = {
                'link':'', # 产品链接
                'name': '', # 产品名称
                'company': '', # 保险公司
                'order_type': order_type, # 排序类型，
                'sales_volume': '', # 销量，
                'comment_number': '',# 评论数，
                'minimum_premium': '', # 最低保费
                'underwriting_age':'', # 承保年龄
                'guarantee_period':'' , #保障期限
                'list': [], #保障项列表
                'website_name': website_name
            }

            # 产品名称和链接
            a = li.find_element_by_class_name("list_pro_tit").find_element_by_tag_name("a")
            name = a.get_attribute("innerHTML")
            # link = a.get_attribute("href")

            print name.strip()
            data['name'] = name.strip()
            # print link

            # 价格
            price1 = li.find_element_by_class_name("price1").get_attribute("innerHTML")
            print price1
            data['minimum_premium'] = price1

            # 销量
            list_pro_price_total = li.find_elements_by_class_name("list_pro_price_total")
            for p in list_pro_price_total:
                con = p.get_attribute("innerHTML")
                if con.find("销量") == -1:
                    pass
                else:
                    print con
                    data['sales_volume'] = con
                    data['comment_number'] = con


            # 点击链接 ， 比 python 自带的稳定很多，
            self.browser.execute_script('$(arguments[0]).click()', a)
            time.sleep(3)
            # 切换窗口
            # print len(self.browser.window_handles)
            for handle in self.browser.window_handles:
                if handle != self.main_window_handle:
                    self.browser.switch_to_window(handle)
                    # 从新打开的页面获取数据，然后关闭窗口
                    link = self.current_url()
                    print link
                    data['link'] = link

                    # 保障内容
                    type_content = self.browser.find_element_by_class_name("type_content")
                    tables = type_content.find_elements_by_tag_name("table")
                    trs = tables[0].find_elements_by_tag_name("tr")

                    lists = []
                    for tr in trs:
                        tds = tr.find_elements_by_tag_name("td")
                        # print len(tds)
                        if len(tds) == 3:
                            print self.strip_tags(tds[0].get_attribute("innerHTML").strip())
                            print self.strip_tags(tds[1].get_attribute("innerHTML").strip())
                            print '----------'
                            item = {
                                'safeguard_term': self.strip_tags(tds[0].get_attribute("innerHTML").strip()),  #保障项
                                'the_sum_insured': self.strip_tags(tds[1].get_attribute("innerHTML").strip()) #保额
                            }
                            lists.append(item)
                    data['list'] = json.dumps(lists)


                    # 保障年龄
                    proChoose = self.browser.find_element_by_class_name("proChoose")

                    # print len(proChoose)
                    # clearfix = proChoose.find_elements_by_class_name("clearfix")
                    divs = proChoose.find_elements_by_tag_name("div")

                    for div in divs:
                        if div.get_attribute("class") == "clearfix":
                            title = div.find_element_by_class_name("tit").get_attribute("innerHTML")
                            # print title

                            if title.find("保障年龄") == -1:
                                pass
                            else:
                                data['underwriting_age'] = self.strip_tags(div.find_element_by_class_name("cur").get_attribute("innerHTML"))

                            # ：
                            if title.find("保障期限") == -1:
                                pass
                            else:
                                data['guarantee_period'] =self.strip_tags(div.find_element_by_class_name("cur").get_attribute("innerHTML"))

                    self.browser.close()
            # 切回主窗口
            self.browser.switch_to_window(self.main_window_handle)

            rows.append(data)
            self.db.insert("bx_list", data)
            self.db.commit()

        return rows

    # 获取当前页面的链接
    def current_url(self):
        js = '''
            var url = window.location.href;
            return url;
        '''
        return self.browser.execute_script(js)


    def delete_title_tag(self):
        js = """
            $("h2").remove();
        """
        self.browser.execute_script(js)


    """
    关闭除主窗口之外的其它窗口，然后切换到主窗口
    """
    def close_window_except_main_window(self):
        for handle in self.browser.window_handles:
                if handle != self.main_window_handle:
                    self.browser.switch_to_window(handle)
                    self.browser.close()
        self.browser.switch_to_window(self.main_window_handle)

    #  打开一个页面，并放入 jquery 框架
    def open_window_with_jquery(self, url):
        # 打开窗口链接
        self.browser.get(url)

        # 注入 jquery
        js = """
            var newNode = document.createElement("script");
            newNode.src = "http://cdn.bootcss.com/jquery/1.11.2/jquery.min.js";
            document.body.appendChild(newNode);
        """
        self.browser.execute_script(js)

    def open_window(self, url):
        self.browser.get(url)
        # self.browser.maximize_window()

    def open_new_window(self, url):
        pass

    def goto_page_bottom(self) :
        # 滑动浏览器滚动条
        js = "var q=document.documentElement.scrollTop=100000"
        self.browser.execute_script(js)
        time.sleep(0.5)
        js = "var q=document.documentElement.scrollTop=500000"
        self.browser.execute_script(js)
        time.sleep(0.5)
        js = "var q=document.documentElement.scrollTop=1000000"
        self.browser.execute_script(js)
        time.sleep(0.5)
        js = "var q=document.documentElement.scrollTop=2000000"
        self.browser.execute_script(js)
        print("go to bottom ...")

    def goto_page_top(self):
        time.sleep(0.5)
        js = "var q=document.documentElement.scrollTop=0"
        self.browser.execute_script(js)
        print("go to top ...")

    def strip_tags(self, string, allowed_tags=''):
        return html2text.html2text(string).strip()
        # return string


if __name__ == '__main__' :
    Instance = FetchWeb()
    Instance.run()


