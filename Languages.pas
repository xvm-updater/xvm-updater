{
    XVM Updater - World of Tanks's XVM one click installer/updater
    Copyright (C) 2013 - 2014  Edgar 'LaCourgette' Fournival

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Languages;

interface

type
  TLanguage = (lngEN, lngFR, lngDE, lngPL, lngRU, lngUA, lngHU, lngFI, lngNL);
  // /!\ Keep the same order in arrays /!\ //

const
  _VERSION_ = 'v2.7';
  LanguageMin: array[TLanguage] of String = ('en', 'fr', 'de', 'pl', 'ru', 'ua', 'hu', 'fi', 'nl');

  sSelectDirectory: array[TLanguage] of String = (
    'Select World of Tanks installation directory:',
    'Sélectionnez le répertoire d''installation de World of tanks :',
    'Wähle dein World of Tanks Hauptverzeichnis aus:',
    'Wskaż ścieżkę dostępu do folderu World_of_Tanks:',
    'Выберите папку с установленной игрой World of Tanks:',
    'Виберіть каталог з інстальованою грою World of Tanks:',
    'Válaszd ki a World of Tanks telepítési mappáját:',
    'Valitse World of Tanks asennus kansio:',
    'Selecteer de World of Tanks installatiemap'
  );

  sFailDirectory: array[TLanguage] of String = (
    'Unable to detect the game in this directory, do you want to try again?',
    'Impossible de détecter le jeu dans ce répertoire, voulez-vous réessayer ?',
    'Das Spiel wurde in diesem Verzeichnis nicht erkannt, willst du es noch einmal versuchen?',
    'Podano nieprawidłową ścieżkę, chcesz spróbować ponownie?',
    'В указанной папке игра не найдена. Хотите выбрать снова?',
    'Не можу знайти гру в каталозі, що ви вказали. Спробувати ще?',
    'A játékot nem sikerült azonosítani a megadott mappában. Szeretnéd újrapróbálni?',
    'Peliä ei löydä tästä kasiosta, haluatko kokeilla toista kansiota?',
    'Geen World of Tanks installatie gevonden in deze map, wilt u het opnieuw proberen?'
  );

  sSpecifyDirectory: array[TLanguage] of String = (
    'Unable to detect World of Tanks installation directory.'+#13#10+'Please specify it manually.',
    'Impossible de détecter le répertoire d''installation de World of Tanks.'+#13#10+'Merci de le spécifier manuellement.',
    'Das Spiel wurde in diesem Verzeichnis nicht erkannt.'+#13#10+'Bitte wähle es Manuell aus.',
    'Podano nieprawidłową ścieżkę.'+#13#10+'Wskaż ją ręcznie.',
    'Не удалось найти папку с World of Tanks.'+#13#10+'Пожалуйста, укажите её вручную.',
    'Не можу знайти каталог World of Tanks.'+#13#10+'Будьласка, вкажіть її вручну.',
    'Nem sikerült megtalálni a World of Tanks telepítési mappáját.'+#13#10+'Kérlek, válaszd ki manuálisan.',
    'World of Tanks asennus kansiota ei löydy.'+#13#10+'Ole hyvä ja kirjoita kansio.',
    'Geen World of Tanks installatie gevonden.'+#13#10+'Geef het handmatig op.'
  );

  sClientRunning: array[TLanguage] of String = (
    'Please exit World of Tanks client before proceeding'+#13#10+'to XVM installation or update.',
    'Merci d''arrêter World of Tanks avant de procéder'+#13#10+'à l''installation ou la mise à jour d''XVM.',
    'Bitte schließe World of Tanks um mit'+#13#10+'der XVM Installation/Update fortzufahren.',
    'Wyłącz grę oraz zamknij klienta gry, zanim przejdziesz'+#13#10+'do instalacji lub aktualizacji XVM.',
    'Пожалуйста, закройте клиент World of Tanks до начала установки'+#13#10+'или обновления XVM.',
    'Будьласка, вийдіть з World of Tanks для, встановлення'+#13#10+'чи оновлення XVM.',
    'Kérlek, zárd be a World of Tanks klienst,'+#13#10+'mielőtt megkezdenéd az XVM telepítését vagy frissítését.',
    'Ole hyvä ja poistu World of Tanks pelistä ennen'+#13#10+'XVM asenusta, tai päivitystä',
    'Sluit de World of Tanks client voordat u verder gaat'+#13#10+'met het installeren of updaten van XVM.'
  );

  sFailModsDir: array[TLanguage] of String = (
    'The installation directory is incorrect or corrupted.'+#13#10+'Unable to check "res_mods" directory content.',
    'Votre répertoire d''installation est incorrect ou corrompu.'+#13#10+'Impossible de vérifier le contenu du dossier "res_mods".',
    'Das Installationsverzeichnis ist Falsch oder Fehlerhaft.'+#13#10+'Der inhalt des Verzeichnisses "res_mods" konnte nicht überprüft werden.',
    'Ścieżka dostępu jest nieprawidłowa lub uszkodzona.'+#13#10+'Sprawdź czy istnieje folder "res_mods" w katalogu głównym.',
    'Папка установки ошибочна или повреждена.'+#13#10+'Не удаётся проверить содержимое папки "res_mods".',
    'Каталог інсталяції помилкова чи пошкоджена.'+#13#10+'Не можу перевірити вміст каталогу "res_mods".',
    'A telepítési mappa nem megfelelő vagy sérült.'+#13#10+'A "res_mods" mappa tartalma nem ellenőrizhető.',
    'Asennus kansio on väärä, tai vahingoittunut'+#13#10+'Ei voi tarkistaa "res_mods" hakemiston sisältöä.',
    'De installatiemap is ongeldig of currupt.'+#13#10+'De inhoud van "res_mods" kon niet gecontroleerd worden.'
  );

  sScriptDownload: array[TLanguage] of String = (
    'Downloading installation script...',
    'Téléchargement du script d''installation...',
    'Downloade Installationsskript...',
    'Pobieranie...',
    'Загрузка скриптов установки...',
    'Завантажую сценарії інсталяції...',
    'Telepítő parancsfájl letöltése...',
    'Ladataan asennustiedostoja...',
    'Installatie script downloaden...'
  );

  sInformationsCollecting: array[TLanguage] of String = (
    'Collecting required information...',
    'Récupération des informations nécessaires...',
    'Erforderliche Informationen werden gesammelt...',
    'Zapisywanie informacji...',
    'Сбор необходимой информации...',
    'Збір необхідної інформації...',
    'Szükséges adatok összegyűjtése...',
    'Kerätään tarvittavia tietoja...',
    'Benodigde informatie verzamelen...'
  );

  sFailDownload: array[TLanguage] of String = (
    'Error whilst downloading:',
    'Une erreur est survenue lors du téléchargement :',
    'Fehler beim Download:',
    'Wystąpił błąd podczas pobierania:',
    'Ошибка загрузки:',
    'Помилка завантаження:',
    'Hiba letöltés közben:',
    'Virhe ladattaessa:',
    'Er is een fout opgetreden tijdens het downloaden:'
  );

  sForcedSource: array[TLanguage] of String = (
    'Script source forced from command line.',
    'Source du script modifiée depuis la ligne de commande.',
    'Das Script wurde vom Benutzer geändert.',
    'Skrypt zmieniony przez użytkownika.',
    'Используется скрипт из параметра командной строки.',
    'Змушені cценарії джерела з командного рядка.',
    'A parancsfájl meg lett változtatva.',
    'Lähde edelleenohjattu komentorivillä.',
    'Script bron geforceerd vanaf de command line.'
  );

  sDefault: array[TLanguage] of String = (
    'Default',
    'Par défaut',
    'Standard',
    'Nowy',
    'Стандартная',
    'За замовчуванням',
    'Alapértelmezett',
    'Oletusasetukset',
    'Standaard'
  );

  sStable: array[TLanguage] of String = (
    'Stable',
    'Stable',
    'Stabile',
    'Bez zmian',
    'Стабильная',
    'Стабільна',
    'Stabil',
    'Vakaa',
    'Stable'
  );

  siWarning: array[TLanguage] of String = (
    'If this is the first time you install XVM or you don''t know the meaning of these options,'+#13#10+'please leave them as they currently are and click directly on "Install / Update".',
    'Si vous installez XVM pour la première fois ou que vous ne savez pas à quoi servent'+#13#10+'ces options, ne modifiez rien et cliquez sur "Installer / Mettre à jour".',
    'Wenn dies das erste Mal ist, dass du XVM installierst oder nicht weißt, was die'+#13#10+'Optionen bewirken, lasse diese wie sie sind und klicke direkt auf ''Installieren / Updaten''.',
    'Jeśli po raz pierwszy instalujesz XVM lub nie znasz znaczenia poniższych opcji,'+#13#10+'pozostaw domyślne ustawienia i kliknij bezpośrednio "Instaluj / Aktualizuj".',
    'Если вы устанавливаете XVM в первый раз или не знаете назначения этих'+#13#10+'настроек, оставьте их как есть и нажмите "Установить / Обновить".',
    'Якщо Ви вперше інсталюєте XVM і не знаете як це робити, залиште всі'+#13#10+'налаштування як є і натисніть кнопку "Інсталювати / Оновити".',
    'Ha most telepítesz először XVM-et, vagy nem tudod, mit változtatnak ezek a'+#13#10+'beállítások, ne módosíts rajtuk, hanem kattints közvetlenül a "Telepítés / Frissítés"-re.',
    'Jos tämä on ensimmäinen kerta, kun asennat XVM:modia ja et tiedä valintojen'+#13#10+'merkitystä, kannattaa valita "Asenna / Päivitä".',
    'Als dit de eerste keer is dat u XVM installeert of u weet niet wat de betekenis van deze'+#13#10+'opties is, laat deze dan voor wat ze zijn en klik meteen op "Installeer / Update".'
  );

  siOptions: array[TLanguage] of String = (
    'Options:',
    'Options :',
    'Optionen:',
    'Opcje:',
    'Настройки:',
    'Налаштування:',
    'Beállítások:',
    'Valinnat:',
    'Opties:'
  );

  siProgress: array[TLanguage] of String = (
    'Progress:',
    'Progression :',
    'Fortschritt:',
    'Przebieg instalacji:',
    'Состояние:',
    'Процес:',
    'Folyamat:',
    'Edistyminen:',
    'Voortgang:'
  );

  siInstallUpdate: array[TLanguage] of String = (
    'Install / Update',
    'Installer / Mettre à jour',
    'Installieren / Updaten',
    'Instaluj / Aktualizuj',
    'Установить / Обновить',
    'Інсталювати / Оновити',
    'Telepítés / Frissítés',
    'Asenna / Päivitä',
    'Installeer / Update'
  );

  siModify: array[TLanguage] of String = (
    'Modify',
    'Modifier',
    'Wechseln',
    'Zmień',
    'Изменить',
    'Змінити',
    'Tallózás',
    'Muokkaa',
    'Bewerken'
  );

  siKeepConfig: array[TLanguage] of String = (
    'Keep old XVM configuration file',
    'Garder l''ancienne configuration d''XVM',
    'Alte XVM Konfiguration behalten',
    'Zachowaj swój dotychczasowy plik konfiguracyjny XVM',
    'Сохранить старый конфиг XVM',
    'Залишити старий файл конфігурації XVM',
    'Előző XVM konfigurációs fájl megtartása',
    'Pidä vanhat XVM asetustiedot',
    'Oude XVM configuratie behouden'
  );

  siShowWinChances: array[TLanguage] of String = (
    'Show win chances in game',
    'Afficher les chances de victoire en jeu',
    'Zeige die Gewinn-chance im Spiel',
    'Pokazuj procent szans na zwycięstwo podczas bitwy',
    'Показывать шансы на победу',
    'Показувати шанси на виграш',
    'Győzelmi esélyek mutatása a játékban',
    'Näytä voittomahdollisuus pelissä',
    'Toon win kansen in het spel'
  );

  siEnableStats: array[TLanguage] of String = (
    'Enable player stats display',
    'Activer l''affichage des statistiques des joueurs',
    'Zeige die Spielerstatistik',
    'Pokazuj statystyki gracza',
    'Включить отображение статистики игроков',
    'Включити статистику гравця',
    'Játékosok teljesítményadatainak megjelenítése',
    'Näytä pelaajien tilastot',
    'Toon speler statistieken'
  );

  siXVMversion: array[TLanguage] of String = (
    'XVM version: ',
    'Version d''XVM : ',
    'XVM Version: ',
    'Wersja XVM: ',
    'Версия XVM: ',
    'XVM версія: ',
    'XVM verzió: ',
    'XVM versio: ',
    'XVM versie: '
  );

  siConfig: array[TLanguage] of String = (
    'Configuration: ',
    'Configuration : ',
    'Konfiguration: ',
    'Plik konfiguracyjny: ',
    'Конфигурация: ',
    'Конфігурація: ',
    'Konfiguráció: ',
    'Asetukset: ',
    'Configuratie: '
  );

  siCurrentAction: array[TLanguage] of String = (
    'Current action: ',
    'Action en cours : ',
    'Wird bearbeitet: ',
    'Trwa: ',
    'Текущая операция: ',
    'Зараз виконується: ',
    'Aktuális művelet: ',
    'Tämänhetkinen toimito: ',
    'Huidige activiteit: '
  );

  siForm: array[TLanguage] of String = (
    'XVM Updater '+_VERSION_+' by LaCourgette', // Thanks blademansw
    'XVM Updater '+_VERSION_+' par LaCourgette',
    'XVM Updater '+_VERSION_+' von LaCourgette [tr. Exekutive]',
    'XVM Updater '+_VERSION_+' by LaCourgette [tł. pokapokami]',
    'XVM Updater '+_VERSION_+' by LaCourgette [tr. wot-ka.ru, Mr.A]',  // Thanks Recnac_UKR, M_r_A
    'XVM Updater '+_VERSION_+' by LaCourgette [tr. wot-ka.ru]',  // Thanks Recnac_UKR
    'XVM Updater '+_VERSION_+' by LaCourgette [tr. buenonacho94]',
    'XVM Updater '+_VERSION_+' by LaCourgette [tr. Seula]',
    'XVM Updater '+_VERSION_+' by LaCourgette [tr. jediah.nl]'
  );

implementation

end.